# Repository Request ------------------


# This file allows you to send an email to the ohdsi-studies github admin requesting for a new
# repository for an OHDSI study. This file uses the R package blastula.
# If you have a gmail smtp you must follow the instructions provided here:
# https://support.google.com/accounts/answer/6010255
# Gmail accounts must allow "Less Secure Apps" to use your Gmail account.

## A) Dependencies ------------------

# install.packages("blastula")
library(blastula)
library(tidyverse, quietly = TRUE)


## B) Variables -----------------

provider <- "gmail" # the smtp provider
senderName <- "{{{ SenderName }}}" # place your name as the sender
senderEmail <- "{{{ SenderEmail }}}" # place your email as the sender
recipientName <- "{{{ RecipientName }}}" # name of recipient
recipientEmail <- "{{{ RecipientEmail }}}" # email of ohdsi-studies repo admin
dateTime <- add_readable_time() # Date and time that email is sent

## C) Setup SMTP -------------------

# You must provide the password for your email account
# If using gmail you need to supply the password for less secure apps

# setup the smtp credentials to send emails
create_smtp_creds_key(
  id = "ulysses_email",
  user = senderEmail,
  provider = provider
)

## D) Create Email ----------------

email <-
  compose_email(
    header = md("## ohdsi-studies Repository Request 📢"),
    body = md(
      glue::glue(
        "Dear {recipientName},

          I would like to start a new OHDSI study! 😀🔬

          Could you please initialize a github repository using the name {{{ RepoName }}} in the ohdsi-studies repository?

          I am using `Ulysses` 🔧 to initialize this OHDSI study.

          Please let me know if you require any additional information about my study. Thank you!

          Best,

          {senderName}"
      )
    ),
    footer = md(glue::glue("Email sent on {date_time}."))
  )


email


## E) Send Email -----------------

email |>
  smtp_send(
    to = senderEmail,
    from = recipientEmail,
    subject = "Requesting new study reposority for {{{ Study }}}",
    credentials = creds_key(id = "ulysses_email")
  )


