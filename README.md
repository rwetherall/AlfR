# alfr

[![Travis build status](https://travis-ci.com/rwetherall/alfr.svg?branch=master)](https://travis-ci.com/rwetherall/alfr)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/rwetherall/alfr?branch=master&svg=true)](https://ci.appveyor.com/project/rwetherall/alfr)
[![Coverage Status](https://coveralls.io/repos/github/rwetherall/alfr/badge.svg?branch=master)](https://coveralls.io/github/rwetherall/alfr?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/alfr)](https://CRAN.R-project.org/package=alfr)
[![](https://cranlogs.r-pkg.org/badges/alfr)](https://cran.r-project.org/package=alfr)

## Overview

Allows you to connect to an 'Alfresco' content management repository and interact with its contents using simple and intuitive functions.

## Getting Started

Before you can interact and manage content you will need access to an 'Alfresco' content management repository.

This [guide](https://community.alfresco.com/community/ecm/pages/get-started) will help you get started with 'Alfresco'.

You will need to know the root URL for the 'Alfresco' content management repository, http://localhost:8080 when installed locally.

## Installation

Install the development version from github with:

```r
# install.packages("remotes")
remotes::install_github("rwetherall/alfr")
```

Install release version from CRAN with:

```r
install.packages("alfr")
```

## Examples

In order to communicate with the Alfresco repository you must first establish a session connection.  The location of the respository is required to do this, along with valid user credentials.

```r
session <- alf_session("http://localhost:8080", "admin", "admin")
```

All content is stored on a repository node so before we can upload binary content we must first create a node to store it within.  In this example we are creating a node called `myFile2.txt` in the folder `test`.  `-root-` is used to reference the root repository node. 

```r
my_file <- alf_node.new(session, "-root-", list(name="myFile2.txt", nodeType="cm:content", relativePath="test"))
```
Now content can be uploaded into the nodes content property where it will be stored in the Alfresco repository.

```r
my_file <- my_file$content$update("resources/testuploaddoc.txt")
```

Get a reference to the node you want to read content from.

```r
my_file <- alf_node.new(session, "-root-", relativePath="test/myFile2.txt")
```

Content stored in a repository node can then be read to the client by opening a connection and reading the contents.

```r
con <- my_file$content$as.file("resources/download.txt") %>% file("r")
content_lines <- readLines(con)
close(con)
```

With a reference to the node that you want to delete from the repository.

```r
alf_node.delete(session, node$id)
```
