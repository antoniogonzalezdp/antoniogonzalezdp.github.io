"""
Entry point for the application.

Using the `if __name__ == "__main__":` guard lets this module act as
both a runnable script and an importable module, so importing it
elsewhere never triggers the app to run unintentionally.
"""

from prepare_order import PrepareOrder

if __name__ == "__main__":
    app = PrepareOrder()
    app.run()
