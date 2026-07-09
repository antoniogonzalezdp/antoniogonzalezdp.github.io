# Fast-Food Order System

Final project for the Python Programming postgraduate certificate (UOC).

## Overview

A small object-oriented Python system that simulates order-taking at a
fast-food business. Cashiers, customers, and products are loaded from
CSV files (standing in for a database), modeled as objects, and used
to build and total an order through a simple command-line flow.

## Setup

```bash
pip install -r requirements.txt
```

## Structure

```
main.py            — entry point
prepare_order.py   — orchestrates the CLI flow
data/               — CSV files: cashiers, customers, products
products/           — Product & FoodPackage class hierarchies
users/               — User base class, Cashier & Customer
util/                — CSVFileManager and Converter classes
orders/              — Order class
```

## Design

- **Abstraction & inheritance** — `Product`, `FoodPackage`, and `User`
  are abstract base classes; concrete types (`Hamburger`, `Soda`,
  `Cashier`, `Customer`...) implement the required methods.
- **Composition** — each `Product` owns a `FoodPackage` instance (a
  `Hamburger` is wrapped, a `Soda` comes in a bottle, and so on).
- **CSV → objects** — `CSVFileManager` reads each CSV into a pandas
  DataFrame; a `Converter` subclass turns each row into a typed object.

## Run

```bash
python main.py
```

You'll be asked for a cashier DNI, a customer DNI, and then a list of
product IDs to build an order.

## Context

Part of a larger portfolio — see the
[project page](https://antoniogonzalezdp.github.io/project-fastfood.html)
for a non-technical walkthrough.
