# Eigenfaces
#### R Project by Marcel Fischer, Ida Jandl and Aaron Osburg
##### within the course "Programming Language R" - Summersemester 2020.

## Table of contents
* [General info](#general-info)
* [Technologies](#technologies)
* [Setup](#setup)
* [Illustration](#illustration)
* [Sources](#sources)


## General info

This Project contains the principle component method on a large set of images of different human faces (here: Olivetti Dataset). The derived eigenvectors are called eigenfaces. They can be used for human face recognition.

## Technologies 
Project is created with:

* R Version: 4.0.2

## Setup 
To run this project, we recommend using RStudio.

---

## Illustration
Nine examples of the images in the Olivetti Dataset:
(plots created with [this code](https://github.com/Osburg/eigenfaces/blob/master/R/visualize_ef.R).)

![Example of Images in Olivetti Dataset](https://github.com/Osburg/eigenfaces/blob/master/inst/extdata/example_images.png)

---

First nine eigenfaces of the Olivetti Dataset:
(plots created with [this code](https://github.com/Osburg/eigenfaces/blob/master/R/visualize_ef.R).)

![Nine Eigenfaces of the Olivetti Dataset](https://github.com/Osburg/eigenfaces/blob/master/inst/extdata/nine_eigenfaces.png)

---

Projection of images onto the first two eigenfaces:
(plots created with [this code](https://github.com/Osburg/eigenfaces/blob/master/R/visualize_ef.R).)

![Projection on eigenfaces](https://github.com/Osburg/eigenfaces/blob/master/inst/extdata/projection_blue.png)
![Projection on eigenfaces](https://github.com/Osburg/eigenfaces/blob/master/inst/extdata/projection_rainbow.png)

---

Furthermore, a classifcation algortihm is implemented. Here it is performed on picture 33 of the dataset.

![3 Closest neighbor](https://github.com/Osburg/eigenfaces/blob/master/inst/extdata/closesttd33.png)

---

Additional content:
* Shiny App
* Vignette 
* Extensive documentation

---

We recommend reading the [vignette](https://github.com/Osburg/eigenfaces/blob/master/vignettes/my-vignette.Rmd) for additional details.

## Sources 
This Project is inspired by Marinovsky F., Wagner P., Gesichtserkennung mit Eigenfaces, FH Zittau/Görlitz


---
