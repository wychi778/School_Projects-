-- USE electronic_sales;


-- CREATE TABLE Customer (
--     CustomerID INT AUTO_INCREMENT PRIMARY KEY,
--     Customer_Name VARCHAR(100)
-- );
-- SELECT * FROM Customer;

--  CREATE TABLE Store (
--     Store_ID INT AUTO_INCREMENT PRIMARY KEY,
--     Store VARCHAR(100),
--     Region VARCHAR(100)
-- );
-- SELECT * FROM Store;

-- CREATE TABLE PaymentMethod (
--     PaymentID INT AUTO_INCREMENT PRIMARY KEY,
--     Payment_Method VARCHAR(50)
-- );
-- SELECT * FROM PaymentMethod;

-- CREATE TABLE Bank (
--     BankID INT AUTO_INCREMENT PRIMARY KEY,
--     BankName VARCHAR(100)
-- );
-- SELECT * FROM Bank;


-- INSERT INTO Bank (BankName)
-- VALUES 
--   ('Chase'),
--   ('Bank of America'),
--   ('Wells Fargo'),
--   ('Citi'),
--   ('No Bank - Cash');

-- ALTER TABLE PaymentMethod
-- ADD COLUMN BankID INT,
-- ADD FOREIGN KEY (BankID) REFERENCES Bank(BankID);
-- SELECT * FROM PaymentMethod;

-- CREATE TABLE Shipping (
--     Shipping_ID INT AUTO_INCREMENT PRIMARY KEY,
--     Shipping_Date DATE,
--     Shipping_Method VARCHAR(50)
-- );

-- INSERT INTO Shipping (Shipping_Date, Shipping_Method)
-- VALUES 
--   ('2024-05-01', 'Standard'),
--   ('2024-05-02', 'Express'),
--   ('2024-05-03', 'Two-Day'),
--   ('2024-05-04', 'Overnight');
-- SELECT * FROM Shipping;

-- CREATE TABLE Supplier (
--     SupplierID INT AUTO_INCREMENT PRIMARY KEY,
--     SupplierName VARCHAR(100),
--     Contact_Info VARCHAR(100)  
-- );
-- INSERT INTO Supplier (SupplierName, Contact_Info)
-- VALUES 
--   ('Samsung', 'samsung_support@samsung.com'),
--   ('Sony', 'contact@sony.com'),
--   ('Apple', 'support@apple.com');
-- SELECT * FROM Supplier;

-- CREATE TABLE Product (
--     Product_ID INT AUTO_INCREMENT PRIMARY KEY,
--     Product VARCHAR(100),
--     SupplierID INT,
--     Category VARCHAR(100),
--     FOREIGN KEY (SupplierID) REFERENCES Supplier(SupplierID)
-- );
-- SELECT * FROM Product;

-- CREATE TABLE Inventory (
--     Inventory_ID INT AUTO_INCREMENT PRIMARY KEY,
--     Store_ID INT,
--     Product_ID INT,
--     Stock_Number INT,
--     FOREIGN KEY (Store_ID) REFERENCES Store(Store_ID),
--     FOREIGN KEY (Product_ID) REFERENCES Product(Product_ID)
-- );
SELECT * FROM Inventory;

-- CREATE TABLE Date (
--     Date_ID INT AUTO_INCREMENT PRIMARY KEY,
--     Sale_Date DATE,
--     Month INT,
--     Quarter INT,
--     Year INT
-- );
-- SELECT * FROM Date;


-- CREATE TABLE Sale (
--     Sale_ID CHAR(36) PRIMARY KEY,
--     CustomerID INT,
--     Store_ID INT,
--     PaymentID INT,
--     Shipping_ID INT,
--     Date_ID INT,
--     Total_Sales DECIMAL(10, 2),  
--     FOREIGN KEY (CustomerID) REFERENCES Customer(CustomerID),
--     FOREIGN KEY (Store_ID) REFERENCES Store(Store_ID),
--     FOREIGN KEY (PaymentID) REFERENCES PaymentMethod(PaymentID),
--     FOREIGN KEY (Shipping_ID) REFERENCES Shipping(Shipping_ID),
--     FOREIGN KEY (Date_ID) REFERENCES Date(Date_ID)
-- );
-- SELECT * FROM Sale;

-- CREATE TABLE Sales_Line (
--     Line_ID INT AUTO_INCREMENT PRIMARY KEY,
--     Sale_ID CHAR(36),
--     Product_ID INT,
--     Date_ID INT,
--     Quantity_Sold INT,
--     Unit_Price DECIMAL(10, 2),
--     Discount_Applied DECIMAL(5, 2), 
--     Store_ID INT,
--     Total_Line_Price DECIMAL(10, 2),
--     FOREIGN KEY (Sale_ID) REFERENCES Sale(Sale_ID),
--     FOREIGN KEY (Product_ID) REFERENCES Product(Product_ID),
--     FOREIGN KEY (Date_ID) REFERENCES Date(Date_ID),
--     FOREIGN KEY (Store_ID) REFERENCES Store(Store_ID)
-- );

-- SET SQL_SAFE_UPDATES = 0;
-- UPDATE Sales_Line
-- SET Total_Line_Price = (Unit_Price * Quantity_Sold) * (1 - Discount_Applied);
-- SELECT * FROM Sales_Line;


-- DROP TABLE IF EXISTS Sales_Line;