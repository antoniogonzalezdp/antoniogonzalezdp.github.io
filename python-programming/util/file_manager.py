# Import the pandas package
import pandas as pd

class CSVFileManager:
    def __init__(self, path: str):
        self.path = path

    def read(self) -> str:
        """
        Reads the CSV file from the given path and returns
        it as a pandas DataFrame.
        """
        return pd.read_csv(self.path)

    def write(self, dataFrame: pd.DataFrame, index: bool = False):
        """
        Writes a DataFrame to the specified CSV path.
        """
        dataFrame.to_csv(self.path, index=index)
