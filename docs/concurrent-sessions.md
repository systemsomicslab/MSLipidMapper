# Concurrent browser sessions

The dashboard creates upload, normalization and analysis state per Shiny session.
Network plot images now use a separate directory and URL prefix per connection.
Closing a connection deletes only its own images. A single static image server
is started for the app and stopped when runApp returns; connection cleanup does
not stop this shared server. Each app invocation also owns a separate image root,
including when api_static_dir is supplied (that parameter specifies the parent).
A static server startup failure is reported at startup instead of silently
continuing with broken image URLs.

This supports isolation of concurrent browser workspaces, not user accounts or
saved sessions. Reconnection after a session expires does not restore analyses.
The default deployment still uses one R process: synchronous computations can
delay other connections. Authentication, authorization for image requests,
persistent workspaces and separate computation workers require additional work.
Image URLs are connection-specific, but are not an authentication boundary.

Validation: Rscript tests/check-session-isolation.R checks the actual dashboard
session initialization code with two connections, identical image names, and
cleanup of one connection while retaining the other's images. Full browser and
HTTP integration checks require Shiny, plumber and the app dependencies.