# Import what's needed to build the abstract User class
from abc import ABC, abstractmethod

class User(ABC):
    def __init__(self, dni: str, name: str, age: int):
        self.dni = dni
        self.name = name
        self.age = age

    @abstractmethod
    def describe(self):
        pass


# Concrete Cashier and Customer implementations:

class Cashier(User):
    def __init__(self, dni: str, name: str, age: int, timeTable: str, salary: float):
        super().__init__(dni, name, age)
        self.timeTable = timeTable
        self.salary = salary

    def describe(self):
        return f"Cashier - Name: {self.name}, DNI: {self.dni}, Timetable: {self.timeTable}, Salary: {self.salary}."


class Customer(User):
    def __init__(self, dni: str, name: str, age: int, email: str, postalcode: str):
        super().__init__(dni, name, age)
        self.email = email
        self.postalcode = postalcode

    def describe(self):
        return f"Customer - Name: {self.name}, DNI: {self.dni}, Age: {self.age}, Email: {self.email}, Postal Code: {self.postalcode}"
