package usersserver

import (
	"encoding/json"
	"net/http"

	"github.com/concourse/concourse/atc"
	"github.com/concourse/concourse/atc/api/present"
)

func (s *Server) GetUsersSince(w http.ResponseWriter, r *http.Request) {
	hLog := s.logger.Session("list-users")
	w.Header().Set("Content-Type", "application/json")

	users, err := s.userFactory.GetAllUsers()
	if err != nil {
		hLog.Error("failed-to-get-users", err)
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	presentedUsers := make([]atc.User, len(users))
	for idx, user := range users {
		presentedUsers[idx] = present.User(user)
	}

	err = json.NewEncoder(w).Encode(presentedUsers)
	if err != nil {
		hLog.Error("failed-to-encode-users", err)
		w.WriteHeader(http.StatusInternalServerError)
	}
	return
}
