# msdial2cytoscape for lipidomics

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)


<!-- badges: end -->

## Overview

msdial2cytoscape for lipidomics



## Run msdial2cytoscape for lipidomics locally

### Step 1: Clone this repository

Open the terminal and run:

``` bash
git clone "https://github.com/takakioka/msdial2cytoscape-for-lipidomics.git"
```

### Step 2: Bulid Docker image

``` bash
docker build -t msdial2cytoscape-for-lipidomics .
```

### Step 3: Run Docker image

Run the container on your terminal once it has been bulied.

``` bash
docker run --rm -p 1028:1028 -p 9000:9000 msdial2cytoscape-for-lipidomics
```

### Step 4: Run msdial2cytoscape for lipidomics in your browser

Open your browser and paste `http://localhost:1028`. 

## Code of Conduct



