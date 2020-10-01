<a href="https://www.taoensso.com" title="More stuff by @ptaoussanis at www.taoensso.com">
<img src="https://www.taoensso.com/taoensso-open-source.png" alt="Taoensso open-source" width="400"/></a>

**[CHANGELOG][]** | [API][] | current [Break Version][]:

```clojure
[com.taoensso/tukey "1.0.0-SNAPSHOT"] ; Early access release
```

<!-- ![build status](https://github.com/ptaoussanis/tukey/workflows/build/badge.svg?branch=master) -->

> See [here][backers] if you're interested in helping support my open-source work, thanks! - Peter Taoussanis

# Tukey: a mini stats toolkit for Clojure/Script

A small, **beginner-friendly** collection of common statistics utils handy for data analysis, machine learning, etc.

**PRs welcome** for additional utils, doc improvements, etc.!

## Features

 * Small, simple cross-platform **all-Clojure/Script codebase**. Handy as a reference.
 * Pleasant, consistant [API][] with **beginner-friendly documentation**.

## Quickstart

Add the necessary dependency to your project:

```clojure
Leiningen: [com.taoensso/tukey "1.0.0-SNAPSHOT"] ; or
deps.edn:   com.taoensso/tukey {:mvn/version "1.0.0-SNAPSHOT"}
```

And setup your namespace imports:

```clojure
(ns my-ns
  (:require [taoensso.tukey :as tukey]))
```

Check out the extensive [API docs][API] for more info from here!

## Contacting me / contributions

Please use the project's [GitHub issues][] for all questions, ideas, etc. **Pull requests welcome**. See the project's [GitHub contributors][] for a list of contributors.

Otherwise, you can reach me at [Taoensso.com][taoensso.com]. Happy hacking!

\- Peter Taoussanis

## License

Distributed under the [EPL v1.0][] (same as Clojure).  
Copyright &copy; 2020 [Peter Taoussanis][taoensso.com].

<!--- Standard links -->
[taoensso.com]: https://www.taoensso.com
[Break Version]: https://github.com/ptaoussanis/encore/blob/master/BREAK-VERSIONING.md

<!--- Standard links (repo specific) -->
[CHANGELOG]: https://github.com/ptaoussanis/tukey/releases
[API]: http://ptaoussanis.github.io/tukey/
[backers]: https://taoensso.com/clojure/backers
[GitHub issues]: https://github.com/ptaoussanis/tukey/issues
[GitHub contributors]: https://github.com/ptaoussanis/tukey/graphs/contributors
[EPL v1.0]: https://raw.githubusercontent.com/ptaoussanis/tukey/master/LICENSE
[Hero]: https://raw.githubusercontent.com/ptaoussanis/tukey/master/hero.png "Title"

<!--- Unique links -->