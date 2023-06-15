# Install keyring - one time operation ---------
install.packages("keyring")

if (Sys.getenv("STRATEGUS_KEYRING_PASSWORD") == "") {
  # set keyring password by adding STRATEGUS_KEYRING_PASSWORD='sos' to renviron
  usethis::edit_r_environ()
  # then add STRATEGUS_KEYRING_PASSWORD='sos', save and close
  # Restart your R Session to confirm it worked
  stop("Please add STRATEGUS_KEYRING_PASSWORD='sos' to your .Renviron file
       via usethis::edit_r_environ() as instructed, save and then restart R session")
}

# Provide your environment specific values ------
# For example...
#dbms <- "redshift"
#connectionString <- "jdbc:redshift://your.server.goes.here:5439/your_cdm_database"
#username <- "username-goes-here"
#password = "password-goes-here"



# Run the rest to setup keyring ----------
##################################
# DO NOT MODIFY BELOW THIS POINT
##################################
keyringName <- "{{{ Name }}}"
keyringPassword <- "{{{ Secret }}" # This password is simply to avoid a prompt when creating the keyring

# Create the keyring if it does not exist.
# If it exists, clear it out so we can re-load the keys
allKeyrings <- keyring::keyring_list()
if (keyringName %in% allKeyrings$keyring) {
  if (keyring::keyring_is_locked(keyring = keyringName)) {
    keyring::keyring_unlock(keyring = keyringName, password = keyringPassword)
  }
  # Delete all keys from the keyring so we can delete it
  cli::cat_bullet("Delete existing keyring: ", keyringName,
                  bullet = "warning", bullet_col = "yellow")
  keys <- keyring::key_list(keyring = keyringName)
  if (nrow(keys) > 0) {
    for (i in 1:nrow(keys)) {
      keyring::key_delete(keys$service[i], keyring = keyringName)
    }
  }
  keyring::keyring_delete(keyring = keyringName)
}
keyring::keyring_create(keyring = keyringName, password = keyringPassword)

# Store the the user-specific configuration -----
keyring::key_set_with_value("dbms", password = dbms, keyring = keyringName)
keyring::key_set_with_value("connectionString", password = connectionString, keyring = keyringName)
keyring::key_set_with_value("username", password = username, keyring = keyringName)
keyring::key_set_with_value("password", password = password, keyring = keyringName)

# Print the values to confirm the configuration
cli::cat_bullet("Keyring Values for ", crayon::green(keyringName),
                bullet = "info", bullet_col = "blue")
keys <- c("dbms", "connectionString", "username", "password")
for (i in seq_along(keys)) {

  cli::cat_line("\t-",  crayon::cyan(keys[i]), ": ", keyring::key_get(keys[i], keyring = keyringName))
}
