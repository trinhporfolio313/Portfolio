-- Import CSV table
CREATE TABLE sales
LIKE train;

SHOW VARIABLES LIKE 'secure_file_priv';

LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/train.csv'
INTO TABLE sales
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

SELECT *
FROM sales;

SELECT COUNT(*) FROM sales;
SELECT COUNT(DISTINCT `Row ID`) FROM sales;

-- Make Copy

CREATE TABLE sales2
LIKE sales;

SELECT *
FROM sales2;

INSERT sales2
SELECT *
FROM sales;

SELECT COUNT(*) FROM sales2;
SELECT COUNT(DISTINCT `Row ID`) FROM sales2;

-- Remove Duplicates

WITH duplicates AS
(SELECT *, 
ROW_NUMBER() OVER(
PARTITION BY `Row ID`, `Order ID`, `Order Date`,
`Ship Date`, `Ship Mode`, `Customer ID`, `Customer Name`,
`Segment`, `Country`, `City`, `State`, `Postal Code`,
`Region`, `Product ID`, `Category`, `Sub-Category`,
`Product Name`, `Sales`) AS row_num
FROM sales2)
SELECT *
FROM duplicates
WHERE row_num > 1;

-- no duplicate found

-- Standardizing Data
SELECT *
FROM sales2;

SELECT `Order ID`
FROM sales2
WHERE LEFT(`Order ID`, 2) != UPPER(LEFT(`Order ID`, 2));

SELECT `Customer ID`
FROM sales2
WHERE LEFT(`Customer ID`, 2) != UPPER(LEFT(`Customer ID`, 2));

SELECT `Row ID`, `Customer Name`
FROM sales2
WHERE `Customer Name` REGEXP '[^A-Za-z0-9 ]';

SELECT *
FROM sales2
WHERE `Customer Name` LIKE '%-%' OR '%-' OR '-%';

SELECT *
FROM sales2
WHERE `Customer Name`= 'Corey-Lock';

UPDATE sales2
SET `Customer Name`= 'Corey Lock'
WHERE `Customer Name`= 'Corey-Lock';

SELECT *
FROM sales2
WHERE `Customer Name` LIKE '%-';

UPDATE sales2
SET `Customer Name`= 'Jason Fortune'
WHERE `Customer Name`= 'Jason Fortune-';

UPDATE sales2
SET `Customer Name`= 'Joy Bell'
WHERE `Customer Name`= 'Joy Bell-';

SELECT `Row ID`,`Order Date`, str_to_date(`Order Date`, '%d/%m/%Y')
FROM sales2;

UPDATE sales2
SET `Order Date`= str_to_date(`Order Date`, '%d/%m/%Y');

SELECT `Order Date`
FROM sales2;

ALTER TABLE sales2
MODIFY COLUMN `Order Date` DATE;

SELECT `Row ID`,`Ship Date`, str_to_date(`Ship Date`, '%d/%m/%Y')
FROM sales2;

UPDATE sales2
SET `Ship Date`= str_to_date(`Ship Date`, '%d/%m/%Y');

SELECT `Ship Date`
FROM sales2;

ALTER TABLE sales2
MODIFY COLUMN `Ship Date` DATE;

-- Remove NULL/Blanks

SELECT *
FROM sales2
WHERE `Postal Code` IS NULL OR `Postal Code` = '';

SELECT `City`, `State`, `Region`,`Postal Code`
FROM sales2
WHERE `City` = 'Burlington' 
AND `State` = 'Vermont'
AND `Region` = 'East' ;

UPDATE sales2
SET `Postal Code` = '05401'
WHERE `City` = 'Burlington' 
AND `State` = 'Vermont'
AND `Region` = 'East' ;

SELECT `City`, `State`, `Region`,`Postal Code`
FROM sales2
WHERE `City` = 'Burlington';

-- Exploratory Data

SELECT *
FROM sales2;

SELECT MAX(`sales`)
FROM sales2;

SELECT *
FROM sales2
WHERE sales = (SELECT MAX(sales) FROM sales2);

SELECT `City`, SUM(`sales`)
FROM sales2
GROUP BY `City`
ORDER BY 2 DESC;

SELECT `State`, SUM(`sales`)
FROM sales2
GROUP BY `State`
ORDER BY 2 DESC;

SELECT `Category`, SUM(`sales`)
FROM sales2
GROUP BY `Category`
ORDER BY 2 DESC;

SELECT `Customer Name`, SUM(`sales`)
FROM sales2
GROUP BY `Customer Name`
ORDER BY 2 DESC;

SELECT `Sub-Category`, SUM(`sales`)
FROM sales2
GROUP BY `Sub-Category`
ORDER BY 2 DESC;

SELECT `Segment`, SUM(`sales`)
FROM sales2
GROUP BY `Segment`
ORDER BY 2 DESC;

SELECT `Product Name`, SUM(`sales`)
FROM sales2
GROUP BY `Product Name`
ORDER BY 2 DESC;

SELECT MIN(`Order Date`), MAX(`Ship Date`)
FROM sales2;

SELECT YEAR(`Order Date`) AS order_year, SUM(`sales`)
FROM sales2
GROUP BY order_year
ORDER BY 1 DESC;

SELECT substring(`Order Date`,1,7) AS order_month, SUM(`sales`)
FROM sales2
GROUP BY order_month
ORDER BY 1 ASC;

WITH Rolling_Total AS
(SELECT substring(`Order Date`,1,7) AS order_month, 
SUM(`sales`) AS total
FROM sales2
GROUP BY order_month
ORDER BY 1 ASC
)
SELECT order_month, total, 
SUM(total) OVER(ORDER BY order_month) AS rolling_total
FROM Rolling_Total;

WITH Customer_Year (Customer, years, total_sales) AS
(SELECT `Customer Name`, YEAR(`Order Date`), SUM(`sales`)
FROM sales2
GROUP BY `Customer Name`, YEAR(`Order Date`)
ORDER BY 3 DESC
), Customer_Year_Rank AS
(SELECT *, DENSE_RANK() OVER(PARTITION BY years ORDER BY total_sales DESC) AS ranking
FROM Customer_Year)
SELECT *
FROM Customer_Year_Rank
WHERE ranking <= 5;

SELECT DAYNAME(`Order Date`) AS Weekday,
       SUM(Sales) AS TotalSales
FROM sales2
GROUP BY Weekday;

SELECT AVG(DATEDIFF(`Ship Date`, `Order Date`)) AS AvgShipDays
FROM sales2;

SELECT `Ship Mode`,
       AVG(DATEDIFF(`Ship Date`, `Order Date`)) AS AvgShipDays
FROM sales2
GROUP BY `Ship Mode`;


