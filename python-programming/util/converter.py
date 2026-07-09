from abc import ABC, abstractmethod

# Import classes from their respective modules
from users.user import Cashier, Customer
from products.product import Hamburger, Soda, Drink, HappyMeal


class Converter(ABC):
    @abstractmethod
    def convert(self, dataFrame, *args) -> list:
        pass

    def print(self, objects):
        for item in objects:
            print(item.describe())


class CashierConverter(Converter):
    def convert(self, dataFrame):
        cashiers = []
        # Iterate over the DataFrame row by row
        for _, row in dataFrame.iterrows():
            # Build a new Cashier object from the row data
            cashier = Cashier(dni=str(row["dni"]), name=row["name"], age=int(row["age"]), timeTable=row["timetable"], salary=float(row["salary"]))
            # Add the cashier to the list of all cashiers
            cashiers.append(cashier)
        return cashiers


class CustomerConverter(Converter):
    # Same logic as the cashier conversion above
    def convert(self, dataFrame):
        customers = []
        for _, row in dataFrame.iterrows():
            customer = Customer(dni=str(row["dni"]), name=row["name"], age=int(row["age"]), email=row["email"], postalcode=row["postalcode"])
            customers.append(customer)
        return customers


class ProductConverter(Converter):
    # Also inherits from Converter; expects a DataFrame of products
    def convert(self, dataFrame) -> list:
        products = []
        # Iterate over the DataFrame rows
        for _, row in dataFrame.iterrows():
            id = row["id"]
            name = row["name"]
            price = float(row["price"])

            # Detect product type from the id prefix
            if id.startswith("HM"):
                product = HappyMeal(id, name, price)
            elif id.startswith("H"):
                product = Hamburger(id, name, price)
            elif id.startswith("G"):
                product = Soda(id, name, price)
            elif id.startswith("B"):
                product = Drink(id, name, price)
            else:
                raise ValueError(f"Unknown product type for id '{id}'")

            # Add the product to the list
            products.append(product)

        return products
