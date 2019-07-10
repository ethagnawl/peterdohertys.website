---
title: Glancing at Python through Ruby-Tinted Glasses
published: true
---

## Glancing at Python through Ruby-Tinted Glasses

I recently shipped the largest, most complex Python project I've ever worked
on. I've written lots of Python scripts and worked on a few Flask/Django
projects, but it had been a few years and those had all been simple REST APIs.
This project was a CMS which allowed admins to produce and publish static web
pages to Google Cloud Storage (GCS)/Google App Engine (GAE). In contrast, the
majority of my development experience over the last few years (er decade, but
who's counting?) has been with Ruby/Rails and AWS/Heroku.

Below you'll find my personal pros/cons list and some misc takes which attempt
to summarize my thoughs and feelings about the Python/GAE/etc. ecosystem. It's
possible/likely that I've overlooked Python/Django/GAE/etc. features and/or am
just wrong about some things, so I'd be very happy to be corrected by anyone
taking the time to read this (PR, comment, whatever you like). For reasons
which were out of my control, it's also worth noting that I was using older
versions of Python (2.7) and Django (1.11) so some of the following may no
longer be relevant.

### Pros

#### Community

The Python community is welcoming, helpful and encouraging. In particular, I
greatly appreciated and valued my interactions with the Djangae community (on
GitHub and the mailing list), /r/djangolearning and Stack Overflow.

##### RTFM
The Python community links directly to source code _much_ more frequently
than the Ruby community does when working through problems, discussing
libraries, etc. This is great and forces you to become intimately familiar with
the libraries you are using. There are many possible reasons that the Ruby
community doesn't do this, but I wonder how much of it is rooted in Ruby's
(historical/perceived) use of metaprogramming or "magic"? Early versions of
Rails used lots of metaprogramming and it wouldn't necessarily be helpful to
reference source when discussing solutions to application level concerns and
it's possible that this has bled out into the larger community, but that's just
a hunch. On the flip side, I've found that the Ruby community favors tutorials,
examples and tests, which are valuable in their own right. Forced to choose,
I'd say that looking at the source code is preferential, but the Python
community could do to include more examples/tutorials.


#### Python

##### Behave
[behave](https://github.com/behave/behave) is a Cucumber-like BDD tool for
Python and, for the most part (see cons entry below), it _Just Worked_. I was
able to write useful, exploratory, high-level, executable specs and found it
easy to stub out collaborating services (GAE Users, Cloud Datastore, etc.) The
OS integration was also very tight and Headless Chrome makes for a fast and,
generally, fantastic execution environment.

##### Flake 8
The flake8 linter is very helpful. It prevents useless arguments about coding
styles and can help you catch defects before you leave your text editor. The
[flake8 Vim plugin](https://github.com/nvie/vim-flake8) works well and
regularly saved me from making silly mistakes.

##### Multiple Inheritance
Multiple inheritance is very powerful and I wish I'd leaned on it more
heavily during this project. An early version of the project I'd worked on had
an explicit God Class which used multiple inheritance to compose its relations
and I, naively, unwound this and made the relations more ... relational. In
retrospect, this was a mistake and, since we were using Cloud Data Storage
(GCP's NoSQL offering and Djangae's default database back-end), I should have
just run with a model hierarchy which mapped cleanly to the object/document
structure of our database.


#### Django

##### Class Based Views
- Django's [CBVs](https://docs.djangoproject.com/en/2.2/topics/class-based-views/)
abstract most common CRUD behavior from Django views (Rails controllers) and
are easy to customize and extend. If I'd have followed the Django conventions
(e.g. model/template locations) more closely, CBVs would have been a drop-in
for many of my view actions. Rails doesn't provide an analogue (there are gems
like [simple_form](https://github.com/plataformatec/simple_form),
[trailblazer](https://github.com/trailblazer/trailblazer) (possibly abandoned?)
and others which do offer similar constructs) and I'm now I'm wishing it did.
(To be fair, Rails' scaffold/generators will generate controller methods that
provide similar behavior, but it'd be nice if those methods were available
through a library or via a uniform interface -- generated code requires
maintenance and makes bugs more likely than the CBV approach does.)

##### Admin
The Django Admin panel is very powerful and, in many cases, can preclude the
need for a custom UI/client app: You define your database and models and it
provides all of the CRUD and GUI glue you could hope for. We'd originally
planned to use Django Admin as our client UI on this project, but our UX
workflow became complicated enough that we quickly ran up against the
limitations of Django Admin's customization options. (See related cons entry
below.)

##### Auth
- Django's contrib.auth module provides a user model, (basic) authentication,
groups and permissions out of the box and it's equivalent to Rails bundling
Devise and CanCan(Can).  IMO, this is a great choice and would save _lots_ of
time, effort and copying/pasting if Rails would mainline these third-party
gems. I haven't gone back to look through Django's history, but it'd be
interesting to see if these features started life outside of Django and were
merged in once they became the de facto patterns or if they were always core
concepts.

##### Template Filters
Django ships with many useful [template filters](https://docs.djangoproject.com/en/2.2/ref/templates/builtins/) (AKA view helpers).
Some standouts are: `autoescape`, `firstof` and `lorem`.


#### Djangae
- Djangae is _extremely_ useful and makes getting a
[new](https://github.com/potatolondon/djangae-scaffold) or existing Django
project up and running on GAE pretty painless. The documentation is great, the
community is very supportive and, for the most part, everything Just Worked.
Djangae provides gobs of utility: installation and configuration of the Google
Cloud SDK and seamless integration with the GC SDK and its various
services (Cloud Datastore, Task Queue, GA users, etc.) from within your Django
app. It's still technically in beta, but it's very solid and, IMO, that
shouldn't stop anyone from using it in their next project.


#### Google Cloud Services

##### AMZ Headers
Google Cloud Services respect some/most/all? AWS (`amz`) headers, which i s
generous and can make transitioning to GCS simple.

##### Cloud Function HTTP Triggers
Cloud Function HTTP triggers are fantastic. At last check, AWS Lambda did not
do this and required users to spin up an API Gateway to achieve similar
behavior -- I'd imagine this is the most popular/desired trigger and it makes
so much sense to provide it by default.

---

### Cons

#### Python

##### Self
In my opinion, one of Python's biggest shortcomings -- especially when using
libraries/frameworks which require OOP -- is having to explicitly pass the
`self` parameter to methods. This is clunky and it's too easy to forget to do
this. Python 3 should have fixed this, while making other breaking changes, by
providing something similar to Ruby, JavaScript, Java, et al's implicit
self/this/that/whatever. Yes, you get used to having to pass self to _every
single instance method_ and yes, you could use tooling to do it automatically,
but why is that necessary? GVR provides some justification for this approach
in [this](https://neopythonic.blogspot.com/2008/10/why-explicit-self-has-to-stay.html) old blog post and likely elsewhere, but the dynamic/metaprogramming edge cases could surely be handled in
other ways.

##### Tuples
- Tuples are great, but their (sometime?) required trailing commas are not.
For example, when passing optargs to a function or creating a tuple with one
member, the trailing comma is required (e.g. `(1,)`). This seems to be
required in order to distinguish the tuple from a set of function arguments
or parenthesis used to emphasize evaluation. This would be fine, if it were
the case for all datatype literals (list, dict, etc.), but this requirement
only applies to tuples. Again, this seems like it could have easily been
resolved by Python 3.

##### Whitespace
Significant whitespace makes programs legible and precludes the need for closing
tags/blocks/etc. Unfortunately, they very often cause programs to run afoul
of (the excellent) flake8 linter. I found myself regularly running over 80
columns as the result of nested dictionaries whose shape/key names were
dictated by a third-party API. Granted, I didn't spend too much time looking
into potential solutions, but IIRC, there is no way to tell the flake8 vim
plugin to ignore long lines.

##### Environment
Getting a Python environment setup is still a little confusing and, despite
the Python community's _there's only one way to do it_ mantra, there are _many_
ways to setup a Python development environment, install dependencies, create
workspaces, etc. FWIW, we settled on virtualenv, pyenv and pip, but pipenv also
looks promising.

##### Dependencies
Pip feels a bit half baked, compared to Bundler/RubyGems. As far as I can
tell, pip doesn't create a lockfile, there's no way to add different
environments (e.g. test, development, production) in a single file and
requirements.txt is relatively unstructured and doesn't provide much/any
metadata to editors/IDEs. (IIRC, pipenv does address these issues and is one
reason I'll consider it in the future. )

##### Missing build tooling
Python doesn't have a build tool (akin to Ruby's Rake) provided by its
standard library. There is a tool called [Paver](https://pypi.org/project/Paver/),
which offers a similar feature set, but doesn't seem to be very widely used.

##### Behave (badly)
As mentioned above, behave is an excellent BDD library. Unfortunately, I was
unable to control its output levels as the library's documentation claimed I'd
be able to (CLI and config file). This resulted in TONS of output (mostly
Djangae warnings, IIRC) when the tests were run and made it very difficult to
follow along or see debug information. Running individual tests helped, but
still resulted in multiple screens full of useless output.

##### Unicode
Python 2.x strings are not Unicode. Casting to/from Unicode must be done
explicitly and with care. If care is not taken, user input can/will result in
parsing errors and program crashes. (Go on, ask me how I know ...)

Python 3's strings _are_ Unicode and this is hailed as one of the defining
features of the new version. It's worth noting, again, that this project used
Python 2 for $REASONS and, since Python 2 will be EOL on 1/1/2020, this problem
is not (as much of) a concern going forward.


#### Django

##### Missing code/file generator
A Rails CLI convenience I sorely missed was `rails generate ...` which can be
used to automatically generate controllers/views/routes/migrations, etc. Its
event lifecycle can be tapped into by third-party libraries to generate tests,
fixtures, documentation, etc. This prevents users from having to copy code from
the documentation to get started and prevents users from having to copy code
between files when creating new views, models, etc.

To be fair, manage.py _may_ provide some of this behavior (I believe it can
create/run migrations), but it doesn't appear to be nearly as comprehensive.
Presumably there are third-party plugins/scripts which automate these common
actions, but it would be nice if Django provided something more robust.  Also,
as mentioned above, CBVs do sort of address this concern, but when you do
actually need concrete classes, it's nice to have them created in a uniform
way.


##### Console
Django missing rails console analogue (need to manually import models, etc.)

##### Application File Structure
Compared to Rails, Django is very unopinionated about application file
structure; naming conventions and seems to encourage (at least to start)
keeping multiple classes (e.g. views) in a single file. This is _okay_, but
reminds me of a problem I've seen in every Sinatra/Express project I've worked
on, wherein every team has to reinvent the wheel when breaking large files with
multiple classes into their component pieces: Do related classes (model, views,
routing) live in an app-level object directory (e.g. app/foo/[view,model].py)
or in a directory structure based on their base class (e.g. app/models/foo.py)?
How are the files named? Are they suffixed/prefixed? etc.

##### Customizing Django Admin
As mentioned above, Django Admin is extremely useful, but there is an upper
limit on how much you can customize it. This is to be expected, but it's
particularly frustrating because it gets you 80% of the way there. Perhaps
there could be a place for a more fleshed out event/hook system or an admin API
(something akin to Headless WordPress?) that'd make it easy to bolt on a custom
front-end?

##### Asset Pipeline
Django doesn't ship with anything akin to Rails' Asset Pipeline. The Asset
Pipeline has been the subject of lots of debate, but it does _Do The Right
Thing_ in most cases and provides tons of utility for free. There are libraries
that you can use to have Django do stylesheet preprocessing, asset
fingerprinting, etc. but you need to spend time researching, integrating and
supporting them.

##### Seed Database
Django does not have an analogue for Rails `db:seed` Rake task. There are
libraries that can be added in order to provide this functionality, but this
seems like a pretty obvious miss.


#### Google Cloud Services

##### Config
GAE seem to encourage placing configuration in files, as opposed to using
environment variables. (Heroku makes it very easy to set ENV vars from the CLI
and the GUI.) IMO, this is very undesirable and can lead to bad behavior
(e.g. committing/pushing sensitive credentials) and complicates deployments.

##### Contextual Search
I found it to be very difficult to find contextual search results when doing
web searches for Google's cloud offerings. Granted, I don't use Google's search
engine, which _may_ be biased towards these results, but trying to find
documentation for "cloud function" or "cloud datastore" using DuckDuckGo proved
to be very frustrating.

##### Missing Documentation
The documentation for the various Google Cloud Platform offerings seems to be
lacking and, again, is hard to search for (see previous point). For instance, I
was never able to figure out how to make a GCS object publicly visible _while_
uploading it using a signed URL. (The only alternatives were to force the user
to make a second request in order to get a new signed URL and then use that URL
in order to make the object public or just make the entire bucket public, which
is what we were, luckily, able to do.) I was also unable to find a conclusive
answer for how long the timeout was for HTTP requests running in background
tasks. It's possible that these were just edge cases, but I saw comments on
Stack Overflow and elsewhere while searching for these answers that seemed to
indicate that other developers were used to seeing these sorts of omissions.

##### Hosting a Static Website
Unfortunately, you can't serve a static site from a Google Cloud Storage
bucket, like you can an AWS S3 bucket. In order to do this, you need to front
bucket with GAE proxy web application which routes the requests, reads the
bucket contents, generates a response, etc.. This seems like a glaring
omission, but we were unable to find a solution that didn't involve standing up
a proxy. Hopefully this is something Google will address in the future, because
it's a deal breaker for me -- S3 more/less does this by default.


#### Misc
- Python, Ruby and friends _all_ feel sloppy after using Elm for a non-trivial
project and experimenting with Rust. Python 3.5 added support for type hints,
which is a step in the right direction and may help make things more robust,
but I don't know if that feature has or will see much adoption.

#### Resources
- https://www.agiliq.com/blog/2010/03/rails-and-django-commands-comparison-and-conversio/
- https://simpleisbetterthancomplex.com/
- https://spapas.github.io/2018/03/19/comprehensive-django-cbv-guide/
- https://medium.com/django-musings/customizing-the-django-admin-site-b82c7d325510
- https://www.techiediaries.com/customize-django-admin/
- https://medium.com/better-programming/strings-unicode-and-bytes-in-python-3-everything-you-always-wanted-to-know-27dc02ff2686
