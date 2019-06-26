package db_test

import (
	"github.com/concourse/concourse/atc/db"
	"time"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("User Factory", func() {

	var err error

	Context("when user doesn't exist", func() {

		It("Insert a user with last_login now()", func() {
			var user db.User
			user, err = userFactory.CreateOrUpdateUser("test", "github")
			Expect(err).ToNot(HaveOccurred())
			Expect(user.Name()).To(Equal("test"))
			Expect(user.LastLogin().Truncate(1 * time.Minute).String()).To(Equal(time.Now().Truncate(1 * time.Minute).String()))
		})
	})
	Context("when username exists but with different connector", func() {

		It("Creates a different user", func() {

			var user1, user2 db.User
			var users []db.User

			user1, err = userFactory.CreateOrUpdateUser("test", "github")
			Expect(err).ToNot(HaveOccurred())
			user2, err = userFactory.CreateOrUpdateUser("test", "basic")
			Expect(err).ToNot(HaveOccurred())
			Expect(user1.ID()).ToNot(Equal(user2.ID()))

			users, err = userFactory.GetAllUsers()
			Expect(err).ToNot(HaveOccurred())
			Expect(users).To(HaveLen(2))
		})
	})

	Context("when username exists and with the same connector", func() {

		It("Doesn't create a different user", func() {

			var users []db.User
			_, err = userFactory.CreateOrUpdateUser("test", "github")
			Expect(err).ToNot(HaveOccurred())
			_, err = userFactory.CreateOrUpdateUser("test", "github")
			Expect(err).ToNot(HaveOccurred())

			users, err = userFactory.GetAllUsers()
			Expect(err).ToNot(HaveOccurred())
			Expect(users).To(HaveLen(1))
		})
	})
})
