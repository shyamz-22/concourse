package db

import (
	"encoding/json"
	"fmt"
	"time"

	"code.cloudfoundry.org/lager"
	sq "github.com/Masterminds/squirrel"
	"github.com/concourse/concourse/atc"
	"github.com/concourse/concourse/atc/db/lock"
)

//go:generate counterfeiter . CheckFactory

type CheckFactory interface {
	PendingChecks() ([]Check, error)
	CreateCheck(int, int, atc.Plan) (Check, error)
	Resources() ([]Resource, error)
	ResourceTypes() ([]ResourceType, error)
	AcquireScanningLock(lager.Logger) (lock.Lock, bool, error)
}

type checkFactory struct {
	conn        Conn
	lockFactory lock.LockFactory
}

func NewCheckFactory(
	conn Conn,
	lockFactory lock.LockFactory,
) CheckFactory {
	return &checkFactory{
		conn:        conn,
		lockFactory: lockFactory,
	}
}

func (c *checkFactory) AcquireScanningLock(
	logger lager.Logger,
) (lock.Lock, bool, error) {
	return c.lockFactory.Acquire(
		logger,
		lock.NewResourceScanningLockID(),
	)
}

func (c *checkFactory) PendingChecks() ([]Check, error) {
	rows, err := checksQuery.
		Where(sq.Eq{
			"status": CheckStatusPending,
		}).
		OrderBy("c.id ASC").
		RunWith(c.conn).
		Query()
	if err != nil {
		return nil, err
	}

	var checks []Check

	for rows.Next() {
		fmt.Println("row")
		check := &check{conn: c.conn, lockFactory: c.lockFactory}

		err := scanCheck(check, rows)
		if err != nil {
			return nil, err
		}

		checks = append(checks, check)
	}

	return checks, nil
}

func (c *checkFactory) CreateCheck(resourceConfigScopeID int, baseResourceTypeID int, plan atc.Plan) (Check, error) {
	tx, err := c.conn.Begin()
	if err != nil {
		return nil, err
	}

	defer Rollback(tx)

	planPayload, err := json.Marshal(plan)
	if err != nil {
		return nil, err
	}

	es := c.conn.EncryptionStrategy()
	encryptedPayload, nonce, err := es.Encrypt(planPayload)
	if err != nil {
		return nil, err
	}

	var id int
	var createTime time.Time
	err = psql.Insert("checks").
		Columns(
			"resource_config_scope_id",
			"base_resource_type_id",
			"schema",
			"status",
			"plan",
			"nonce",
		).
		Values(
			resourceConfigScopeID,
			baseResourceTypeID,
			schema,
			CheckStatusPending,
			encryptedPayload,
			nonce,
		).
		Suffix(`
			ON CONFLICT DO NOTHING
			RETURNING id, create_time
		`).
		RunWith(tx).
		QueryRow().
		Scan(&id, &createTime)
	if err != nil {
		return nil, err
	}

	err = tx.Commit()
	if err != nil {
		return nil, err
	}

	return &check{
		id:                    id,
		resourceConfigScopeID: resourceConfigScopeID,
		baseResourceTypeID:    baseResourceTypeID,
		schema:                schema,
		status:                CheckStatusPending,
		plan:                  plan,
		createTime:            createTime,

		conn:        c.conn,
		lockFactory: c.lockFactory,
	}, err
}

func (c *checkFactory) Resources() ([]Resource, error) {
	var resources []Resource

	rows, err := resourcesQuery.
		Where(sq.Eq{"p.paused": false}).
		RunWith(c.conn).
		Query()

	defer Close(rows)

	for rows.Next() {
		r := &resource{
			conn:        c.conn,
			lockFactory: c.lockFactory,
		}

		err = scanResource(r, rows)
		if err != nil {
			return nil, err
		}

		resources = append(resources, r)
	}

	return resources, nil
}

func (c *checkFactory) ResourceTypes() ([]ResourceType, error) {
	var resourceTypes []ResourceType

	rows, err := resourceTypesQuery.
		RunWith(c.conn).
		Query()

	defer Close(rows)

	for rows.Next() {
		r := &resourceType{
			conn:        c.conn,
			lockFactory: c.lockFactory,
		}

		err = scanResourceType(r, rows)
		if err != nil {
			return nil, err
		}

		resourceTypes = append(resourceTypes, r)
	}

	return resourceTypes, nil
}
