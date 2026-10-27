## Resubmission

This is an update (1.3.1) that fixes the test failures reported on the
CRAN check results page for version 1.3.0.

* The tests submit jobs to the live 'PCATS' web API. They failed on CRAN
  while the API was temporarily unavailable (it returned HTTP 403). The tests
  are now skipped on CRAN, when offline, and when the API cannot be reached.
* API calls no longer fail with "argument is of length zero" when the server
  returns an unexpected (non-JSON or non-200) response; such responses are now
  retried and then reported as a failed request.

## R CMD check results

0 errors | 0 warnings | 0 notes

* local macOS 27.0.1, R 4.6.1
