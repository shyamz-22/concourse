package db

//go:generate counterfeiter . UserFactory

type UserFactory interface {
	CreateOrUpdateUser(username, connector string) (User, error)
}

type userFactory struct {
	conn Conn
}

func (f *userFactory) CreateOrUpdateUser(username, connector string) (User, error) {
	tx, err := f.conn.Begin()

	if err != nil {
		return nil, err
	}
	defer Rollback(tx)

	u, found, err := user{
		name:      username,
		connector: connector,
	}.find(tx)

	if err != nil {
		return nil, err
	}

	if found {
		err = user{
			id: u.ID(),
		}.delete(tx)
		if err != nil {
			return nil, err
		}
	}
	u, err = user{
		name:      username,
		connector: connector,
	}.create(tx)

	if err != nil {
		return nil, err
	}

	return u, nil



	//row, err := psql.Select("id").
	//	From("users").
	//	Where(sq.Eq{
	//		"username":  userInfo.Name,
	//		"connector": userInfo.Connector,
	//	}).
	//	RunWith(tx).
	//	Query()
	//if err != nil {
	//	return err
	//}
	//defer Close(row)
	//
	//if err != nil {
	//	return err
	//}
	//if row.Next() {
	//	_, err := psql.Delete("users").
	//		Where(sq.Eq{
	//			"username":  userInfo.Name,
	//			"connector": userInfo.Connector,
	//		}).
	//		RunWith(tx).
	//		Exec()
	//	if err != nil {
	//		return err
	//	}
	//}
	//_, err = psql.Insert("users").
	//	Columns("username", "connector").
	//	Values(userInfo.Name, userInfo.Connector).
	//	RunWith(tx).
	//	Exec()
	//if err != nil {
	//	return err
	//}
	//
	//err = tx.Commit()
	//return err
}

func NewUseractory(conn Conn) UserFactory {
	return &userFactory{
		conn: conn,
	}
}
