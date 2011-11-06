**An easy way to send mail via http GET/POST request**

**Usage:**

    curl http://localhost:8000/mailto/<email>/<subject> -d "<content>"

e.g.

    curl http://localhost:8000/mailto/youraddress@mail.com/Hello -d "Hello sendmail-via-http"
