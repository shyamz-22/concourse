package db_test

import (
	"github.com/concourse/concourse/atc/db"
	"time"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)
var _= Describe("User Factory", func() {
	// when user is not exited in the table `users`, insert a user with `last_login` now()

	BeforeEach(func() {
	})

	FContext("when user doesn't exist", func() {
		It("Insert a user with last_login now()", func () {
			var user db.User
			var err error
			user, err = userFactory.CreateOrUpdateUser("test", "github")
			Expect(err).ToNot(HaveOccurred())
			Expect(user.Name()).To(Equal("test"))
			Expect(user.LastLogin().Truncate(1*time.Minute).String()).To(Equal(time.Now().Truncate(1*time.Minute).String()))
		})
	})
	FContext("when username exists but with different connector", func() {
		It("Creates a different user", func () {
			var user1, user2 db.User
			var err error
			user1, err = userFactory.CreateOrUpdateUser("test", "github")
			Expect(err).ToNot(HaveOccurred())
			user2, err = userFactory.CreateOrUpdateUser("test", "basic")
			Expect(err).ToNot(HaveOccurred())
			Expect(user1.ID()).ToNot(Equal(user2.ID()))
		})
	})
	FContext("when username exists but with different connector", func() {
		It("Creates a different user", func () {
			var user1, user2 db.User
			var err error
			user1, err = userFactory.CreateOrUpdateUser("test", "github")
			Expect(err).ToNot(HaveOccurred())
			user2, err = userFactory.CreateOrUpdateUser("test", "github")
			Expect(err).ToNot(HaveOccurred())
			//doesn't work, need to create function get all users and make sure it returns only 1 user
			Expect(user1.ID()).To(Equal(user2.ID()))
		})
	})
})