
;;;### (autoloads (url-register-auth-scheme url-get-authentication) "url-auth" "lisp/url-auth.el")

(autoload 'url-get-authentication "url-auth" "\
Return an authorization string suitable for use in the WWW-Authenticate
header in an HTTP/1.0 request.

URL    is the url you are requesting authorization to.  This can be either a
       string representing the URL, or the parsed representation returned by
       `url-generic-parse-url'
REALM  is the realm at a specific site we are looking for.  This should be a
       string specifying the exact realm, or nil or the symbol 'any' to
       specify that the filename portion of the URL should be used as the
       realm
TYPE   is the type of authentication to be returned.  This is either a string
       representing the type (basic, digest, etc), or nil or the symbol 'any'
       to specify that any authentication is acceptable.  If requesting 'any'
       the strongest matching authentication will be returned.  If this is
       wrong, it's no big deal, the error from the server will specify exactly
       what type of auth to use
PROMPT is boolean - specifies whether to ask the user for a username/password
       if one cannot be found in the cache

arguments: (URL REALM TYPE PROMPT &optional ARGS)
" nil nil)

(autoload 'url-register-auth-scheme "url-auth" "\
Register an HTTP authentication method.

TYPE     is a string or symbol specifying the name of the method.
         This should be the same thing you expect to get returned in
         an Authenticate header in HTTP/1.0 - it will be downcased.
FUNCTION is the function to call to get the authorization information.
         This defaults to `url-?-auth', where ? is TYPE.
RATING   a rating between 1 and 10 of the strength of the authentication.
         This is used when asking for the best authentication for a specific
         URL.  The item with the highest rating is returned.

arguments: (TYPE &optional FUNCTION RATING)
" nil nil)

;;;***

;;;### (autoloads (url-cache-expired url-cache-extract url-is-cached url-store-in-cache) "url-cache" "lisp/url-cache.el")

(autoload 'url-store-in-cache "url-cache" "\
Store buffer BUFF in the cache.

arguments: (&optional BUFF)
" nil nil)

(autoload 'url-is-cached "url-cache" "\
Return non-nil if the URL is cached.
The actual return value is the last modification time of the cache file.

arguments: (URL)
" nil nil)

(autoload 'url-cache-extract "url-cache" "\
Extract FNAM from the local disk cache.

arguments: (FNAM)
" nil nil)

(autoload 'url-cache-expired "url-cache" "\
Return t if a cached file has expired.

arguments: (URL MOD)
" nil nil)

;;;***

;;;### (autoloads (url-cid) "url-cid" "lisp/url-cid.el")

(autoload 'url-cid "url-cid" "\


arguments: (URL)
" nil nil)

;;;***

;;;### (autoloads (url-dav-vc-registered url-dav-supported-p) "url-dav" "lisp/url-dav.el")

(autoload 'url-dav-supported-p "url-dav" "\


arguments: (URL)
" nil nil)

(autoload 'url-dav-vc-registered "url-dav" "\


arguments: (URL)
" nil nil)

;;;***

;;;### (autoloads (url-file) "url-file" "lisp/url-file.el")

(autoload 'url-file "url-file" "\
Handle file: and ftp: URLs.

arguments: (URL CALLBACK CBARGS)
" nil nil)

;;;***

;;;### (autoloads (url-open-stream url-gateway-nslookup-host) "url-gw" "lisp/url-gw.el")

(autoload 'url-gateway-nslookup-host "url-gw" "\
Attempt to resolve the given HOST using nslookup if possible.

arguments: (HOST)
" t nil)

(autoload 'url-open-stream "url-gw" "\
Open a stream to HOST, possibly via a gateway.
Args per `open-network-stream'.
Will not make a connection if `url-gateway-unplugged' is non-nil.
Might do a non-blocking connection; use `process-status' to check.

arguments: (NAME BUFFER HOST SERVICE)
" nil nil)

;;;***

;;;### (autoloads (url-insert-file-contents url-file-local-copy url-copy-file url-file-handler url-handler-mode) "url-handlers" "lisp/url-handlers.el")

(autoload 'url-handler-mode "url-handlers" "\
Use URL to handle URL-like file names." t nil)

(autoload 'url-file-handler "url-handlers" "\
Function called from the `file-name-handler-alist' routines.
OPERATION is what needs to be done (`file-exists-p', etc).  ARGS are
the arguments that would have been passed to OPERATION.

arguments: (OPERATION &rest ARGS)
" nil nil)

(autoload 'url-copy-file "url-handlers" "\
Copy URL to NEWNAME.  Both args must be strings.
Signals a `file-already-exists' error if file NEWNAME already exists,
unless a third argument OK-IF-ALREADY-EXISTS is supplied and non-nil.
A number as third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.
Fourth arg KEEP-TIME non-nil means give the new file the same
last-modified time as the old one.  (This works on only some systems.)
Fifth arg PRESERVE-UID-GID is ignored.
A prefix arg makes KEEP-TIME non-nil.

arguments: (URL NEWNAME &optional OK-IF-ALREADY-EXISTS KEEP-TIME PRESERVE-UID-GID)
" nil nil)

(autoload 'url-file-local-copy "url-handlers" "\
Copy URL into a temporary file on this machine.
Returns the name of the local copy, or nil, if FILE is directly
accessible.

arguments: (URL &rest IGNORED)
" nil nil)

(autoload 'url-insert-file-contents "url-handlers" "\


arguments: (URL &optional VISIT BEG END REPLACE)
" nil nil)

;;;***

;;;### (autoloads (url-http-options url-http-file-attributes url-http-file-exists-p url-http) "url-http" "lisp/url-http.el")

(autoload 'url-http "url-http" "\
Retrieve URL via HTTP asynchronously.
URL must be a parsed URL.  See `url-generic-parse-url' for details.
When retrieval is completed, the function CALLBACK is executed with
CBARGS as the arguments.

arguments: (URL CALLBACK CBARGS)
" nil nil)

(autoload 'url-http-file-exists-p "url-http" "\


arguments: (URL)
" nil nil)

(defalias 'url-http-file-readable-p 'url-http-file-exists-p)

(autoload 'url-http-file-attributes "url-http" "\


arguments: (URL &optional ID-FORMAT)
" nil nil)

(autoload 'url-http-options "url-http" "\
Return a property list describing options available for URL.
This list is retrieved using the `OPTIONS' HTTP method.

Property list members:

methods
  A list of symbols specifying what HTTP methods the resource
  supports.

dav
  A list of numbers specifying what DAV protocol/schema versions are
  supported.

dasl
  A list of supported DASL search types supported (string form)

ranges
  A list of the units available for use in partial document fetches.

p3p
  The `Platform For Privacy Protection' description for the resource.
  Currently this is just the raw header contents.  This is likely to
  change once P3P is formally supported by the URL package or
  Emacs/W3.

arguments: (URL)
" nil nil)

(defconst url-https-default-port 443 "\
Default HTTPS port.")

(defconst url-https-asynchronous-p t "\
HTTPS retrievals are asynchronous.")
 (autoload 'url-default-expander "url-expand")

(defalias 'url-https-expand-file-name 'url-default-expander)
 (autoload 'url-https "url-http")
 (autoload 'url-https-file-exists-p "url-http")
 (autoload 'url-https-file-readable-p "url-http")
 (autoload 'url-https-file-attributes "url-http")

;;;***

;;;### (autoloads (url-irc) "url-irc" "lisp/url-irc.el")

(autoload 'url-irc "url-irc" "\


arguments: (URL)
" nil nil)

;;;***

;;;### (autoloads (url-ldap) "url-ldap" "lisp/url-ldap.el")

(autoload 'url-ldap "url-ldap" "\
Perform an LDAP search specified by URL.
The return value is a buffer displaying the search results in HTML.
URL can be a URL string, or a URL vector of the type returned by
`url-generic-parse-url'.

arguments: (URL)
" nil nil)

;;;***

;;;### (autoloads (url-mailto url-mail) "url-mailto" "lisp/url-mailto.el")

(autoload 'url-mail "url-mailto" "\


arguments: (&rest ARGS)
" t nil)

(autoload 'url-mailto "url-mailto" "\
Handle the mailto: URL syntax.

arguments: (URL)
" nil nil)

;;;***

;;;### (autoloads (url-data url-generic-emulator-loader url-info url-man) "url-misc" "lisp/url-misc.el")

(autoload 'url-man "url-misc" "\
Fetch a Unix manual page URL.

arguments: (URL)
" nil nil)

(autoload 'url-info "url-misc" "\
Fetch a GNU Info URL.

arguments: (URL)
" nil nil)

(autoload 'url-generic-emulator-loader "url-misc" "\


arguments: (URL)
" nil nil)

(defalias 'url-rlogin 'url-generic-emulator-loader)

(defalias 'url-telnet 'url-generic-emulator-loader)

(defalias 'url-tn3270 'url-generic-emulator-loader)

(autoload 'url-data "url-misc" "\
Fetch a data URL (RFC 2397).

arguments: (URL)
" nil nil)

;;;***

;;;### (autoloads (url-snews url-news) "url-news" "lisp/url-news.el")

(autoload 'url-news "url-news" "\


arguments: (URL)
" nil nil)

(autoload 'url-snews "url-news" "\


arguments: (URL)
" nil nil)

;;;***

;;;### (autoloads (url-ns-user-pref url-ns-prefs isInNet isResolvable dnsResolve dnsDomainIs isPlainHostName) "url-ns" "lisp/url-ns.el")

(autoload 'isPlainHostName "url-ns" "\


arguments: (HOST)
" nil nil)

(autoload 'dnsDomainIs "url-ns" "\


arguments: (HOST DOM)
" nil nil)

(autoload 'dnsResolve "url-ns" "\


arguments: (HOST)
" nil nil)

(autoload 'isResolvable "url-ns" "\


arguments: (HOST)
" nil nil)

(autoload 'isInNet "url-ns" "\


arguments: (IP NET MASK)
" nil nil)

(autoload 'url-ns-prefs "url-ns" "\


arguments: (&optional FILE)
" nil nil)

(autoload 'url-ns-user-pref "url-ns" "\


arguments: (KEY &optional DEFAULT)
" nil nil)

;;;***

;;;### (autoloads (url-generic-parse-url url-recreate-url) "url-parse" "lisp/url-parse.el")

(autoload 'url-recreate-url "url-parse" "\
Recreate a URL string from the parsed URLOBJ.

arguments: (URLOBJ)
" nil nil)

(autoload 'url-generic-parse-url "url-parse" "\
Return an URL-struct of the parts of URL.
The CL-style struct contains the following fields:
TYPE USER PASSWORD HOST PORTSPEC FILENAME TARGET ATTRIBUTES FULLNESS.

arguments: (URL)
" nil nil)

;;;***

;;;### (autoloads (url-setup-privacy-info) "url-privacy" "lisp/url-privacy.el")

(autoload 'url-setup-privacy-info "url-privacy" "\
Setup variables that expose info about you and your system.

arguments: ()
" t nil)

;;;***

;;;### (autoloads (url-view-url url-truncate-url-for-viewing url-file-extension url-hexify-string url-unhex-string url-parse-query-string url-file-nondirectory url-file-directory url-percentage url-display-percentage url-pretty-length url-strip-leading-spaces url-eat-trailing-space url-get-normalized-date url-lazy-message url-normalize-url url-insert-entities-in-string url-parse-args url-debug url-debug) "url-util" "lisp/url-util.el")

(defvar url-debug nil "\
*What types of debug messages from the URL library to show.
Debug messages are logged to the *URL-DEBUG* buffer.

If t, all messages will be logged.
If a number, all messages will be logged, as well shown via `message'.
If a list, it is a list of the types of messages to be logged.")

(autoload 'url-debug "url-util" "\


arguments: (TAG &rest ARGS)
" nil nil)

(autoload 'url-parse-args "url-util" "\


arguments: (STR &optional NODOWNCASE)
" nil nil)

(autoload 'url-insert-entities-in-string "url-util" "\
Convert HTML markup-start characters to entity references in STRING.
Also replaces the \" character, so that the result may be safely used as
  an attribute value in a tag.  Returns a new string with the result of the
  conversion.  Replaces these characters as follows:
    &  ==>  &amp;
    <  ==>  &lt;
    >  ==>  &gt;
    \"  ==>  &quot;

arguments: (STRING)
" nil nil)

(autoload 'url-normalize-url "url-util" "\
Return a 'normalized' version of URL.
Strips out default port numbers, etc.

arguments: (URL)
" nil nil)

(autoload 'url-lazy-message "url-util" "\
Just like `message', but is a no-op if called more than once a second.
Will not do anything if `url-show-status' is nil.

arguments: (&rest ARGS)
" nil nil)

(autoload 'url-get-normalized-date "url-util" "\
Return a 'real' date string that most HTTP servers can understand.

arguments: (&optional SPECIFIED-TIME)
" nil nil)

(autoload 'url-eat-trailing-space "url-util" "\
Remove spaces/tabs at the end of a string.

arguments: (X)
" nil nil)

(autoload 'url-strip-leading-spaces "url-util" "\
Remove spaces at the front of a string.

arguments: (X)
" nil nil)

(autoload 'url-pretty-length "url-util" "\


arguments: (N)
" nil nil)

(autoload 'url-display-percentage "url-util" "\


arguments: (FMT PERC &rest ARGS)
" nil nil)

(autoload 'url-percentage "url-util" "\


arguments: (X Y)
" nil nil)

(defalias 'url-basepath 'url-file-directory)

(autoload 'url-file-directory "url-util" "\
Return the directory part of FILE, for a URL.

arguments: (FILE)
" nil nil)

(autoload 'url-file-nondirectory "url-util" "\
Return the nondirectory part of FILE, for a URL.

arguments: (FILE)
" nil nil)

(autoload 'url-parse-query-string "url-util" "\


arguments: (QUERY &optional DOWNCASE ALLOW-NEWLINES)
" nil nil)

(autoload 'url-unhex-string "url-util" "\
Remove %XX embedded spaces, etc in a URL.
If optional second argument ALLOW-NEWLINES is non-nil, then allow the
decoding of carriage returns and line feeds in the string, which is normally
forbidden in URL encoding.

arguments: (STR &optional ALLOW-NEWLINES)
" nil nil)

(autoload 'url-hexify-string "url-util" "\
Return a new string that is STRING URI-encoded.
First, STRING is converted to utf-8, if necessary.  Then, for each
character in the utf-8 string, those found in `url-unreserved-chars'
are left as-is, all others are represented as a three-character
string: \"%\" followed by two lowercase hex digits.

arguments: (STRING)
" nil nil)

(autoload 'url-file-extension "url-util" "\
Return the filename extension of FNAME.
If optional argument X is t, then return the basename
of the file with the extension stripped off.

arguments: (FNAME &optional X)
" nil nil)

(autoload 'url-truncate-url-for-viewing "url-util" "\
Return a shortened version of URL that is WIDTH characters wide or less.
WIDTH defaults to the current frame width.

arguments: (URL &optional WIDTH)
" nil nil)

(autoload 'url-view-url "url-util" "\
View the current document's URL.
Optional argument NO-SHOW means just return the URL, don't show it in
the minibuffer.

This uses `url-current-object', set locally to the buffer.

arguments: (&optional NO-SHOW)
" t nil)

;;;***

;;;### (autoloads (url-retrieve-synchronously url-retrieve) "url" "lisp/url.el")

(autoload 'url-retrieve "url" "\
Retrieve URL asynchronously and call CALLBACK with CBARGS when finished.
URL is either a string or a parsed URL.

CALLBACK is called when the object has been completely retrieved, with
the current buffer containing the object, and any MIME headers associated
with it.  It is called as (apply CALLBACK STATUS CBARGS).
STATUS is a list with an even number of elements representing
what happened during the request, with most recent events first,
or an empty list if no events have occurred.  Each pair is one of:

\(:redirect REDIRECTED-TO) - the request was redirected to this URL
\(:error (ERROR-SYMBOL . DATA)) - an error occurred.  The error can be
signaled with (signal ERROR-SYMBOL DATA).

Return the buffer URL will load into, or nil if the process has
already completed (i.e. URL was a mailto URL or similar; in this case
the callback is not called).

The variables `url-request-data', `url-request-method' and
`url-request-extra-headers' can be dynamically bound around the
request; dynamic binding of other variables doesn't necessarily
take effect.

arguments: (URL CALLBACK &optional CBARGS)
" nil nil)

(autoload 'url-retrieve-synchronously "url" "\
Retrieve URL synchronously.
Return the buffer containing the data, or nil if there are no data
associated with it (the case for dired, info, or mailto URLs that need
no further processing).  URL is either a string or a parsed URL.

arguments: (URL)
" nil nil)

;;;***

(provide 'url-auto)
