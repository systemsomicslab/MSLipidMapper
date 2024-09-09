# MSDIAL2Cytoscape

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)


<!-- badges: end -->

## Overview

MSDIAL2Cytoscape



## Run MSDIAL2Cytoscape locally

### Step 1: Clone this repository

Open the terminal and run:

``` bash
git clone "https://github.com/takakioka/lipidomics-viz"
```

### Step 2: Bulid Docker image

``` bash
docker build -t MSDIAL2Cytoscape .
```

### Step 3: Run Docker image

Run the container on your terminal once it has been bulied.

``` bash
docker run --rm -p 1028:1028 -p 9000:9000 MSDIAL2Cytoscape
```

### Step 4: Run MSDIAL2Cytoscape in your browser

Open your browser and paste `http://localhost:1028`. 

## Code of Conduct

Please note that the MSDIAL2Cytoscape project is released with a [Contributor
Code of

By contributing to this project, you agree to abide by its terms.

