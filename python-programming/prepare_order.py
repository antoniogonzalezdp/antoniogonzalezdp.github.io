"""
Fast-food order system — final project for the Python Programming
postgraduate certificate (UOC).

A small object-oriented system that reads cashiers, customers, and
products from CSV files, models them as objects (abstract base
classes + inheritance for products, packaging, and users), and lets
a cashier build and total an order for a customer through a simple
command-line flow.

Package layout:
    products/  — Product and FoodPackage class hierarchies
    users/     — User base class, Cashier and Customer subclasses
    util/      — CSVFileManager (CSV <-> DataFrame) and Converter classes
    orders/    — Order class (add products, calculate total, display)

See README.md for setup and usage.
"""

# Packages and modules to import
from users.user import Cashier, Customer
from products.product import Hamburger, Soda, Drink, HappyMeal
from util.converter import CashierConverter, CustomerConverter, ProductConverter
from util.file_manager import CSVFileManager
from orders.order import Order


class PrepareOrder:
    # Initialize attributes
    def __init__(self):
        # Read and convert the cashier and customer CSVs
        self.cashiers = CashierConverter().convert(CSVFileManager("data/cashiers.csv").read())
        self.customers = CustomerConverter().convert(CSVFileManager("data/customers.csv").read())

        # Read and convert the product CSVs
        self.products = []
        self.products += ProductConverter().convert(CSVFileManager("data/hamburgers.csv").read())
        self.products += ProductConverter().convert(CSVFileManager("data/sodas.csv").read())
        self.products += ProductConverter().convert(CSVFileManager("data/drinks.csv").read())
        self.products += ProductConverter().convert(CSVFileManager("data/happyMeal.csv").read())

    # Lookup helpers for cashier, customer, and product

    def find_cashier_by_dni(self, dni: str):
        for cashier in self.cashiers:
            if cashier.dni == dni:
                return cashier
        return None

    def find_customer_by_dni(self, dni: str):
        for customer in self.customers:
            if customer.dni == dni:
                return customer
        return None

    def find_product_by_id(self, pid: str):
        for product in self.products:
            if product.id == pid:
                return product
        return None

    # Display the list of available products
    def show_products(self):
        for product in self.products:
            print(product.describe())

    def run(self):
        # Ask for the cashier and customer DNI; abort if either isn't found
        dni_cashier = str(input("Enter the cashier's DNI: "))
        cashier = self.find_cashier_by_dni(dni_cashier)
        if not cashier:
            print("Error: Cashier not found.")
            return

        dni_customer = input("Enter the customer's DNI: ")
        customer = self.find_customer_by_dni(dni_customer)
        if not customer:
            print("Error: Customer not found.")
            return

        # Create the order for the selected cashier and customer
        order = Order(cashier, customer)

        # Show the available products to choose from
        print("\nAvailable products:")
        self.show_products()

        while True:
            # Loop asking the user for a product id;
            # pressing Enter with no id ends the loop
            pid = input("Enter product ID: ")
            if pid == "":
                break
            # Look up the product by id; add it to the order if found, otherwise show an error
            product = self.find_product_by_id(pid)
            if product:
                order.add(product)
                print(f"Product '{product.name}' added.")
            else:
                print("Error: Product not found.")

        # Display the full order: customer, cashier, product list, and final price
        print("\nOrder summary:")
        order.show()
