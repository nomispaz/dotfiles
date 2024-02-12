'''
    Copyright (C) 2024  Simon Heise

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

'''

import sqlite3
from os import sys, path
from PyQt6 import uic
from datetime import date, datetime
from PyQt6.QtGui import QFont
from PyQt6.QtWidgets import (
        QMainWindow,
        QApplication
)

############################################################

class QtCalWindow(QMainWindow):

    def showAppointment(self, date):
        selectedDate = str(date.getDate()[0]) + "-" + str(date.getDate()[1]) + "-" + str(date.getDate()[2])
        print(selectedDate)

    def setupUIfunctions(self):
        self.calendarWidget.clicked.connect(self.showAppointment)
        

    def __init__(self):
        super(QtCalWindow, self).__init__()
        self.absolute_path = path.dirname(__file__)
        uic.loadUi(path.join(self.absolute_path, "config/ui/QtCalWindow.ui"),self)
        self.setupUIfunctions()

class qtcal():

    # You need one (and only one) QApplication instance per application.
    # Pass in sys.argv to allow command line arguments for your app.
    # If you know you won't use command line arguments QApplication([]) works too.
    qtcalApp = QApplication(sys.argv)
    qtcalApp.setFont(QFont('Awesome', 16))

    # Create a Qt widget, which will be our window.
    qtcalWindow = QtCalWindow()
    # IMPORTANT!!!!! Windows are hidden by default.
    qtcalWindow.show()

    # Start the event loop.
    sys.exit(qtcalApp.exec())