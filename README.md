![Haskell CI](https://github.com/iconnect/ridley/workflows/Haskell%20CI/badge.svg?branch=master)

_"Quick metrics to grow your production app healthy & strong."_

![scotts-porage-oats](https://cloud.githubusercontent.com/assets/442035/19548306/10c828d2-969e-11e6-86b7-0672655b518f.jpg)

## Ridley

Ridley is a library which allow you to easily expose the most common Haskell metrics to [Prometheus](https://prometheus.io).
By default, with minimal configuration, this is what you get by default:

* GHC Conc metrics
* EKG metrics
* Process memory
* CPU Avg Load
* Disk Usage
* Network Usage
* Wai endpoints status codes

On top of that, it's possible to define custom metrics so you can embed your own in your monitored programs. A collection
of optional metrics can be found in the complementary package `ridley-extras`, which adds things like:

* Open file descriptors
