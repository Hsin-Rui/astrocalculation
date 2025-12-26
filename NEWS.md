# astrocalculation 0.1.8 -- 2025-12-26
* Bug fix in se_path setting
* Implement unit test for planetary position calculation (test case: Brad Pitt)

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


