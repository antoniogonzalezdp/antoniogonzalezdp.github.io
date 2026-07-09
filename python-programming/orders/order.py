from users.user import *
from products.product import *

class Order:
    def __init__(self, cashier: Cashier, customer: Customer):
        self.cashier = cashier
        self.customer = customer
        self.products = []

    def add(self, product: Product):
        # Add a product to the order
        self.products.append(product)

    def calculateTotal(self) -> float:
        # Sum the price of all products in the order
        return sum(p.price for p in self.products)

    def show(self):
        # Display all the order information
        print("Hello : " + self.customer.describe())
        print("Was attended by : " + self.cashier.describe())
        for product in self.products:
            print(product.describe())
        print(f"Total price : {self.calculateTotal()}")
