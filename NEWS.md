# pcatsAPIclientR 1.3.1

* Tests that call the live PCATS API are skipped on CRAN and when the API is unreachable.
* API calls no longer fail with "argument is of length zero" when the server returns an unexpected (non-JSON or non-200) response; non-200 responses are now retried.

# pcatsAPIclientR 1.3.0

Retries during API interaction.

# pcatsAPIclientR 1.2.0

Better error handling.

# pcatsAPIclientR 1.1.0

This release incorporates latest API changes.

## Breaking changes

* `staticGP()` and `dynamicGP()` functions now accepts time argument(s).

# pcatsAPIclientR 1.0.0

* Initial release.

