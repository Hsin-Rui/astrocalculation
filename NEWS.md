# astrocalculation 0.1.15 -- 2026-4-6
* Update auth_reset_password to unlock account after resetting password
* reduce amount of CICD checks (leave out windows and ubuntu devel)
* Improve logging (log user id etc.)

# astrocalculation 0.1.14 -- 2026-1-6
* Implement password lock logic

# astrocalculation 0.1.13 -- 2026-1-4
* Implement backend logic for reset password

# astrocalculation 0.1.12 -- 2025-12-30
* Account authentification using google
* Implement restore_session method in DataManager to improve cookie handling logic.

# astrocalculation 0.1.11 -- 2025-12-29
* Enable backend to send verification email.
* Implement e-mail verification at registration.

# astrocalculation 0.1.10.9000 -- 2025-12-28
* Add two frontend helpers to get Chinese names as labels

# astrocalculation 0.1.10 -- 2025-12-28
* Implement traditional Chinese name (name_zh) in the cities_db.sqlite for all countries and 22 Taiwanese cities
* update lookup_city_data to enable plotting chart for all cities in cities_db.sqlite

# astrocalculation 0.1.9 -- 2025-12-27
* Bug fix in add_datetime and minus_datetime (match astro.shiny)
* Expand R6 DataManager to handle add and minus datetime
* Delete legacy scripts

# astrocalculation 0.1.8 -- 2025-12-26
* Bug fix in se_path setting
* Implement unit test for planetary position calculation (test case: Brad Pitt)
* Implement logger

# astrocalculation 0.1.7 -- 2025-12-26
* Expand the R6 object and db utils to allow delete chart from user library
* Implemented unit tests for user library

# astrocalculation 0.1.6.9003 -- 2025-12-26
* Bug fix in db_utils (save user profile and get profile)

# astrocalculation 0.1.6.9002 -- 2025-12-26
* Implement session authorization in R6 class

# astrocalculation 0.1.6.9001 -- 2025-12-25
* Bug fix in database logic (owner_id --> user_entity_id)

# astrocalculation 0.1.6.9000 -- 2025-12-25
* Correct initialization of database connection in R6 object

# astrocalculation 0.1.6 -- 2025-12-25
* Extend database schema for handling user registration, login and logs
* Implement user registration and login logic

# astrocalculation 0.1.5 -- 2025-12-24
* Expand R6 data manager to handle user profile management
* First logic saving user profile data

# astrocalculation 0.1.4 -- 2025-12-21
* initialize postgres db (local db for development)

# astrocalculation 0.1.3 -- 2025-12-13
* export add_datetime and minus_datetime

# astrocalculation 0.1.2 -- 2025-11-25
* debugged draw_whole_sign_chart

# astrocalculation 0.1.1 -- 2025-11-24
* activate renv.lock

# astrocalculation 0.1.0 -- 2025-07-27
* remove cities.rda and replace it with cities.sqlite
* update chart visualization and chart calculation, using longitude and latitude directly.
* Affected: DataManager, calculate_planet_position, draw_whole_sign_chart


