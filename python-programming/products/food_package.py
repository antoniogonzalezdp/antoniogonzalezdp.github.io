# Import ABC and abstractmethod to build the abstract FoodPackage class
from abc import ABC, abstractmethod

class FoodPackage(ABC):
    @abstractmethod
    def pack(self) -> str:
        pass
    @abstractmethod
    def material(self) -> str:
        pass
    def describe(self):
        return f"Package: {self.pack()}, Material: {self.material()}"


# Each concrete class below defines its own pack type and material

class Wrapping(FoodPackage):
    def pack(self):
        return "Food Wrap Paper"
    def material(self):
        return "Aluminium"


class Bottle(FoodPackage):
    def pack(self):
        return "Beverage Container"
    def material(self):
        return "Plastic"


class Glass(FoodPackage):
    def pack(self):
        return "Drinking Glass"
    def material(self):
        return "Plastic"


class Box(FoodPackage):
    def pack(self):
        return "Food container box"
    def material(self):
        return "Cardboard"
