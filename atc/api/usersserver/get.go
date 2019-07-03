package usersserver

import (
	"encoding/json"
	"github.com/concourse/concourse/atc"
	"github.com/concourse/concourse/atc/api/present"
	"github.com/concourse/concourse/atc/db"
	"net/http"
	"time"
)

const SINCE = "since"

func (s *Server) GetUsersSince(w http.ResponseWriter, r *http.Request) {
	hLog := s.logger.Session("list-users")
	w.Header().Set("Content-Type", "application/json")

	var (
		users []db.User
		err error
	)

	err = r.ParseForm()
	if err != nil {
		hLog.Error("failed-to-parse-form-data", err)
		w.WriteHeader(http.StatusBadRequest)
		return
	}

	if isSinceSet(r) {
		tmSince, err := time.Parse("2006-1-02", r.FormValue(SINCE))
		if err != nil {
			hLog.Error("failed-to-parse-time", err)
			w.WriteHeader(http.StatusBadRequest)
			if err = json.NewEncoder(w).Encode(map[string]string{"error": "wrong date format (yyyy-MM-dd)"}); err != nil {
				hLog.Error("failed-to-encode-date-parsing-error", err)
				w.WriteHeader(http.StatusInternalServerError)
			}
			return
		}
		users, err = s.userFactory.GetAllUsersByLoginDate(tmSince)
		if err != nil {
			hLog.Error("failed-to-get-users-since", err)
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
	} else {
		users, err = s.userFactory.GetAllUsers()
		if err != nil {
			hLog.Error("failed-to-get-users", err)
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
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

func isSinceSet(r *http.Request) bool {
	if len(r.FormValue(SINCE)) > 0 {
		return true
	}
	return false
}