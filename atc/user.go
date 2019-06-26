package atc

import "time"

type User struct {
	ID int
	Username string
	Connector string
	LastLogin time.Time
}
