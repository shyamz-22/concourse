package api_test

import (
	"github.com/concourse/concourse/atc/api/accessor/accessorfakes"
	"net/http"
	"net/url"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = FDescribe("Users API", func() {

	var (
		response   *http.Response
		fakeaccess *accessorfakes.FakeAccess
		query      url.Values
	)

	BeforeEach(func() {
		fakeaccess = new(accessorfakes.FakeAccess)
	})

	Context("GET /api/v1/users", func() {

		JustBeforeEach(func() {
			fakeAccessor.CreateReturns(fakeaccess)

			req, err := http.NewRequest("GET", server.URL+"/api/v1/users", nil)
			Expect(err).NotTo(HaveOccurred())

			req.URL.RawQuery = query.Encode()

			response, err = client.Do(req)
			Expect(err).NotTo(HaveOccurred())
		})

		Context("when authenticated", func() {

			BeforeEach(func() {
				fakeaccess.IsAuthenticatedReturns(true)
			})

			Context("not an admin", func() {

				It("returns 403", func() {
					Expect(response.StatusCode).To(Equal(http.StatusForbidden))
				})

			})

			Context("being an admin", func() {

				BeforeEach(func() {
					fakeaccess.IsAdminReturns(true)
				})

				It("succeeds", func() {
					Expect(response.StatusCode).To(Equal(http.StatusOK))
				})

				It("returns all users logged in since table creation", func() {

				})

			})

		})

		Context("not authenticated", func() {

			BeforeEach(func() {
				fakeaccess.IsAuthenticatedReturns(false)
			})

			It("returns 401", func() {
				Expect(response.StatusCode).To(Equal(http.StatusUnauthorized))
			})

		})

	})

})
