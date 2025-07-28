#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# run_monthly_sentiment.R
# ---------------------------------------------------------------------------
# * Renders monthly_tweet_report.Rmd  → HTML
# * Prints the HTML to PDF (pagedown + headless Chrome)
# * Uploads the PDF to Supabase    (bucket: monthly‑sentiment/YYYYMM/…)
# * Emails the PDF via Mailjet
# ---------------------------------------------------------------------------

## ── 0. Packages ────────────────────────────────────────────────────────────
required <- c(
  "tidyverse","tidytext","lubridate","stringi","kableExtra",
  "forcats","widyr","ggraph","igraph",
  "data.table",
  "sentimentr",
  "rmarkdown","pagedown","knitr",
  "jsonlite","httr2","DBI","RPostgres","base64enc"
)
invisible(lapply(required, \(p){
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, quiet = TRUE)
  library(p, character.only = TRUE)
}))
`%||%` <- function(a,b){ if (isTRUE(is.na(a)) || (is.character(a)&&!nzchar(a))) b else a }

## ── 1. Config / env vars ───────────────────────────────────────────────────
# MONTH_START may be blank. If blank, use *previous* month so the report
# always covers a full calendar month even when run on the 1st.
m_env        <- Sys.getenv("MONTH_START")
month_start  <- suppressWarnings(as.Date(m_env)) %||%
  lubridate::floor_date(Sys.Date() - 1, unit = "month") -   # previous month’s 1st
  months(0)                                                # same day handling
month_end    <- lubridate::ceiling_date(month_start, "month") - 1        # last day of month

RMD_FILE <- "monthly_tweet_report.Rmd"
HTML_OUT <- "monthly_tweet_report.html"
PDF_OUT  <- "monthly_tweet_report.pdf"

SB_URL         <- Sys.getenv("SUPABASE_URL")
SB_STORAGE_KEY <- Sys.getenv("SUPABASE_SERVICE_ROLE")
SB_BUCKET      <- "monthly-sentiment"

MJ_API_KEY    <- Sys.getenv("MJ_API_KEY")
MJ_API_SECRET <- Sys.getenv("MJ_API_SECRET")
MAIL_FROM     <- Sys.getenv("MAIL_FROM")
MAIL_TO       <- Sys.getenv("MAIL_TO")

stopifnot(
  SB_URL      != "", SB_STORAGE_KEY != "",
  MJ_API_KEY  != "", MJ_API_SECRET  != "",
  MAIL_FROM   != "", MAIL_TO        != ""
)

## ── 2. Knit Rmd → HTML ─────────────────────────────────────────────────────
rmarkdown::render(
  input       = RMD_FILE,
  output_file = HTML_OUT,
  params      = list(month_start = month_start),
  quiet       = TRUE
)

## ── 3. HTML → PDF (pagedown) ───────────────────────────────────────────────
chrome_path <- Sys.getenv("CHROME_BIN")
if (!nzchar(chrome_path)) chrome_path <- pagedown::find_chrome()

pagedown::chrome_print(
  input      = HTML_OUT,
  output     = PDF_OUT,
  browser    = chrome_path,
  extra_args = c("--headless=new","--disable-gpu","--no-sandbox"),
  timeout    = 20000
)
if (!file.exists(PDF_OUT)) stop("❌ PDF not generated – ", PDF_OUT, " missing")

## ── 4. Upload PDF to Supabase storage ──────────────────────────────────────
iso_folder <- format(month_start, "%Y%m")                       # e.g. 202507
file_name  <- sprintf("%s_to_%s.pdf",
                      format(month_start,"%Y-%m-%d"),
                      format(month_end  ,"%Y-%m-%d"))
object_path <- file.path(iso_folder, file_name)

upload_url <- sprintf("%s/storage/v1/object/%s/%s?upload=1",
                      SB_URL, SB_BUCKET, object_path)

request(upload_url) |>
  req_method("POST") |>
  req_headers(
    Authorization  = sprintf("Bearer %s", SB_STORAGE_KEY),
    `x-upsert`     = "true",
    `Content-Type` = "application/pdf"
  ) |>
  req_body_file(PDF_OUT) |>
  req_perform() |>
  resp_check_status()

cat("✔ Uploaded to Supabase:", object_path, "\n")

## ── 5. Email the PDF via Mailjet ───────────────────────────────────────────
if (str_detect(MAIL_FROM, "<.+@.+>")) {
  from_email <- str_remove_all(str_extract(MAIL_FROM, "<.+@.+>"), "[<>]")
  from_name  <- str_trim(str_remove(MAIL_FROM, "<.+@.+>$"))
} else {
  from_email <- MAIL_FROM
  from_name  <- "Sentiment Bot"
}

mj_resp <- request("https://api.mailjet.com/v3.1/send") |>
  req_auth_basic(MJ_API_KEY, MJ_API_SECRET) |>
  req_body_json(list(
    Messages = list(list(
      From        = list(Email = from_email, Name = from_name),
      To          = list(list(Email = MAIL_TO)),
      Subject     = sprintf(
        "Monthly Sentiment Report – %s",
        format(month_start, "%B %Y")
      ),
      TextPart    = "Attached is the monthly Twitter sentiment report.",
      Attachments = list(list(
        ContentType   = "application/pdf",
        Filename      = file_name,
        Base64Content = base64enc::base64encode(PDF_OUT)
      ))
    ))
  )) |>
  req_perform()

if (resp_status(mj_resp) >= 300) {
  cat("Mailjet error body:\n",
      resp_body_string(mj_resp, encoding = "UTF-8"), "\n")
  stop("❌ Mailjet returned status ", resp_status(mj_resp))
}

cat("📧  Mailjet response OK — report emailed\n")
