
<!-- PROJECT SHIELDS -->

[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MS-PL License][license-shield]][license-url]
[![LinkedIn][linkedin-shield]][linkedin-url]



<!-- PROJECT LOGO -->
<br />
<p align="center">
  <a href="https://github.com/lisp-stat/plot">
    <img src="https://lisp-stat.dev/images/stats-image.svg" alt="Logo" width="80" height="80">
  </a>

  <h3 align="center">Plot</h3>

  <p align="center">
	A library for plotting with Common Lisp
	<br />
    <a href="https://lisp-stat.dev/docs/manuals/plot"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/lisp-stat/plot/issues">Report Bug</a>
    ·
    <a href="https://github.com/lisp-stat/plot/issues">Request Feature</a>
    ·
    <a href="https://lisp-stat.github.io/plot/">Reference Manual</a>
  </p>
</p>



<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary><h2 style="display: inline-block">Table of Contents</h2></summary>
  <ol>
    <li>
      <a href="#about-the-project">About the Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
	<li><a href="#resources">Resources</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About the Project

  The Plot system provides a way to visualise data.  It includes
  functions for text-based plotting that work in the REPL, and
  JavaScript visualisations that are rendered in a browser.  It is
  designed as a front end 'grammar', modeled after that _Grammar of
  Graphics_ and implemented with various back ends.  Currently
  Vega-Lite is complete as a backend.  It is integrated with
  [data-frame](https://github.com/Lisp-Stat/data-frame), and can also
  be used independently.


### Built With

* [cl-who](https://github.com/edicl/cl-who)
* [cl-spark](https://github.com/tkych/cl-spark)
* [yason](https://phmarek.github.io/yason/)
* [LASS](https://github.com/Shinmera/LASS)


<!-- GETTING STARTED -->
## Getting Started

To get a local copy up and running follow these steps:

### Prerequisites

An ANSI Common Lisp implementation. Developed and tested with
[SBCL](https://www.sbcl.org/) and
[CCL](https://github.com/Clozure/ccl).

### Installation

To make the system accessible to [ASDF](https://common-lisp.net/project/asdf/) (a build facility, similar to `make` in the C world), clone the repository in a directory ASDF knows about.  By default the `common-lisp` directory in your home directory is known. Create this if it doesn't already exist and then:

1. Clone the repositories
```sh
cd ~/common-lisp && \
git clone https://github.com/Lisp-Stat/plot
```

2. From the REPL reset the ASDF source-registry to find the new systems:
   ```lisp
   (asdf:clear-source-registry)
   ```
3. Load the system
   ```lisp
   (asdf:load-system :plot/vega)
   ```

If you have installed the slime ASDF extensions, you can invoke this
with a comma (',') from the slime REPL.

### Getting dependencies

To get the third party systems that these system may depend on you can use a dependency manager, such as [Quicklisp](https://www.quicklisp.org/beta/) or [CLPM](https://www.clpm.dev/) Once installed, get the dependencies with either of:

```lisp
(clpm-client:sync :sources "clpi") ;sources may vary
```

```lisp
(ql:quickload :plot)
```

You need do this only once. After obtaining the dependencies, you can
load the system with `ASDF` as described above without first syncing
sources.

<!-- USAGE EXAMPLES -->
## Usage

For examples, please refer to the
[Documentation](https://lisp-stat.dev/docs/plot/plotting).


<!-- ROADMAP -->
## Roadmap

See the [open issues](https://github.com/lisp-stat/plot/issues) for a list of proposed features and known issues.

## Resources

This system is part of the [Lisp-Stat](https://lisp-stat.dev/)
project; that should be your first stop for information. Also see the
[resources](https://lisp-stat.dev/resources) and
[community](https://lisp-stat.dev/community) pages for more
information.

<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to be learn, inspire, and create. Any contributions you make are **greatly appreciated**. Please see [CONTRIBUTING](CONTRIBUTING.md) for details on the code of conduct, and the process for submitting pull requests.

<!-- LICENSE -->
## License

Distributed under the MS-PL License. See [LICENSE](LICENSE) for more information.


<!-- CONTACT -->
## Contact

Project Link: [https://github.com/lisp-stat/plot](https://github.com/lisp-stat/plot)



<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/lisp-stat/plot.svg?style=for-the-badge
[contributors-url]: https://github.com/lisp-stat/plot/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/lisp-stat/plot.svg?style=for-the-badge
[forks-url]: https://github.com/lisp-stat/plot/network/members
[stars-shield]: https://img.shields.io/github/stars/lisp-stat/plot.svg?style=for-the-badge
[stars-url]: https://github.com/lisp-stat/plot/stargazers
[issues-shield]: https://img.shields.io/github/issues/lisp-stat/plot.svg?style=for-the-badge
[issues-url]: https://github.com/lisp-stat/plot/issues
[license-shield]: https://img.shields.io/github/license/lisp-stat/plot.svg?style=for-the-badge
[license-url]: https://github.com/lisp-stat/plot/blob/master/LICENSE
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://www.linkedin.com/company/symbolics/
