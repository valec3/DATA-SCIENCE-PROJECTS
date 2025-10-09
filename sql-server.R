


library(DBI)
library(odbc)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(scales)
library(readr)
library(ggrepel)



con <- dbConnect(odbc(),
                 Driver   = "ODBC Driver 18 for SQL Server",  
                 Server   = "DESKTOP-O3VTK74",                    
                 Database = "AdventureWorks2022",
                 UID      = "valece-1",
                 PWD      = "valece", 
                 TrustServerCertificate = "yes", 
                 Port = 1433)                           


# Probar conexión
dbGetQuery(con, "SELECT TOP 5 * FROM Sales.SalesOrderHeader")

# -------------------------
# 1) CHEQUEOS / LIMPIEZA
# -------------------------

# Función para chequear nulos por columnas (SQL Server)
check_nulls <- function(con, table, cols){
  null_exprs <- paste0("SUM(CASE WHEN ", cols, " IS NULL THEN 1 ELSE 0 END) AS ", cols, "_nulls", collapse = ", ")
  q <- sprintf("SELECT COUNT(*) AS total_rows, %s FROM %s", null_exprs, table)
  dbGetQuery(con, q)
}

# Ejemplos para tablas clave
check_nulls(con, "Sales.SalesOrderHeader", c("OrderDate","ShipDate","TotalDue","SalesPersonID","CustomerID","TerritoryID"))
check_nulls(con, "Sales.SalesOrderDetail", c("SalesOrderID","ProductID","UnitPrice","UnitPriceDiscount","LineTotal","OrderQty"))
check_nulls(con, "Production.Product", c("ProductID","Name","ProductSubcategoryID"))
check_nulls(con, "Sales.Customer", c("CustomerID","StoreID","PersonID","TerritoryID"))

# Buscar duplicados simples en PKs (si hubiera)
dup_query <- function(con, table, pk){
  q <- sprintf("SELECT %s, COUNT(*) AS n FROM %s GROUP BY %s HAVING COUNT(*) > 1", pk, table, pk)
  dbGetQuery(con, q)
}
dup_query(con, "Sales.SalesOrderHeader", "SalesOrderID")         # normalmente vacío
dup_query(con, "Sales.SalesOrderDetail", "SalesOrderDetailID")   # si existe

# -------------------------
# 2) VENTAS MENSUALES (time series)
# -------------------------
monthly_sql <- "
SELECT YEAR(OrderDate) as yr, MONTH(OrderDate) as mth,
       SUM(TotalDue) as total_sales, COUNT(SalesOrderID) as n_orders
FROM Sales.SalesOrderHeader
GROUP BY YEAR(OrderDate), MONTH(OrderDate)
ORDER BY YEAR(OrderDate), MONTH(OrderDate);
"
monthly_sales <- dbGetQuery(con, monthly_sql) %>%
  mutate(date = as.Date(paste0(yr, "-", sprintf('%02d', mth), "-01"))) %>%
  arrange(date)

# Plot: ventas mensuales
p_monthly <- ggplot(monthly_sales, aes(x = date, y = total_sales)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  scale_y_continuous(labels = scales::label_number(prefix = "S/ ", big.mark = ",")) +
  labs(title = "Ventas mensuales (TotalDue)", x = "Mes", y = "Ventas (S/)") +
  theme_minimal()
print(p_monthly)
ggsave("ventas_mensuales.png", p_monthly, width = 10, height = 5)

# -------------------------
# 3) TOP PRODUCTOS POR VENTAS
# -------------------------
top_products_sql <- "
SELECT TOP 15
  p.ProductID,
  p.Name AS ProductName,
  pc.Name AS Category,
  SUM(sod.LineTotal) AS total_sales,
  SUM(sod.OrderQty) AS qty_sold
FROM Sales.SalesOrderDetail sod
JOIN Production.Product p ON sod.ProductID = p.ProductID
LEFT JOIN Production.ProductSubcategory psc ON p.ProductSubcategoryID = psc.ProductSubcategoryID
LEFT JOIN Production.ProductCategory pc ON psc.ProductCategoryID = pc.ProductCategoryID
GROUP BY p.ProductID, p.Name, pc.Name
ORDER BY SUM(sod.LineTotal) DESC;
"
top_products <- dbGetQuery(con, top_products_sql)

# Plot: barras horizontales top productos
p_top_products <- ggplot(top_products, aes(reorder(ProductName, total_sales), total_sales)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::label_number(prefix = "S/ ", big.mark = ",")) +
  labs(title = "Top 15 productos por ventas", x = "Producto", y = "Ventas (S/)") +
  theme_minimal()
print(p_top_products)
ggsave("top_productos.png", p_top_products, width = 10, height = 6)

# -------------------------
# 4) VENTAS POR TERRITORIO
# -------------------------
territory_sql <- "
SELECT t.TerritoryID, t.Name AS TerritoryName, t.CountryRegionCode,
       SUM(soh.TotalDue) AS total_sales, COUNT(soh.SalesOrderID) AS orders_count
FROM Sales.SalesOrderHeader soh
LEFT JOIN Sales.SalesTerritory t ON soh.TerritoryID = t.TerritoryID
GROUP BY t.TerritoryID, t.Name, t.CountryRegionCode
ORDER BY SUM(soh.TotalDue) DESC;
"
sales_territory <- dbGetQuery(con, territory_sql)

p_territory <- ggplot(sales_territory, aes(x = reorder(TerritoryName, total_sales), y = total_sales, fill = CountryRegionCode)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::label_number(prefix = "S/ ", big.mark = ",")) +
  labs(title = "Ventas por territorio", x = "Territorio", y = "Ventas (S/)") +
  theme_minimal() +
  guides(fill = guide_legend(title = "País/Código"))
print(p_territory)
ggsave("ventas_territorio.png", p_territory, width = 10, height = 6)

# -------------------------
# 5) VENTAS POR VENDEDOR (SalesPerson)
# -------------------------
salesperson_sql <- "
SELECT TOP 20 sp.BusinessEntityID,
       pp.FirstName + ' ' + pp.LastName AS SalesPersonName,
       SUM(soh.TotalDue) AS total_sales,
       COUNT(soh.SalesOrderID) AS orders
FROM Sales.SalesPerson sp
JOIN Person.Person pp ON sp.BusinessEntityID = pp.BusinessEntityID
JOIN Sales.SalesOrderHeader soh ON sp.BusinessEntityID = soh.SalesPersonID
GROUP BY sp.BusinessEntityID, pp.FirstName, pp.LastName
ORDER BY SUM(soh.TotalDue) DESC;
"
salesperson <- dbGetQuery(con, salesperson_sql)

p_salesperson <- ggplot(salesperson, aes(reorder(SalesPersonName, total_sales), total_sales)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::label_number(prefix = "S/ ", big.mark = ",")) +
  labs(title = "Top 20 vendedores por ventas", x = "Vendedor", y = "Ventas (S/)") +
  theme_minimal()
print(p_salesperson)
ggsave("ventas_vendedores.png", p_salesperson, width = 10, height = 7)

# -------------------------
# 6) COMPORTAMIENTO DE CLIENTES
# -------------------------
customers_sql <- "
SELECT c.CustomerID,
       COALESCE(p.FirstName + ' ' + p.LastName, 'Store ' + CAST(c.StoreID AS VARCHAR(10))) AS CustomerName,
       COUNT(soh.SalesOrderID) AS orders,
       SUM(soh.TotalDue) AS total_spent,
       AVG(soh.TotalDue) AS avg_order,
       MIN(soh.OrderDate) AS first_order,
       MAX(soh.OrderDate) AS last_order
FROM Sales.Customer c
LEFT JOIN Person.Person p ON c.PersonID = p.BusinessEntityID
LEFT JOIN Sales.SalesOrderHeader soh ON c.CustomerID = soh.CustomerID
GROUP BY c.CustomerID, p.FirstName, p.LastName, c.StoreID
HAVING COUNT(soh.SalesOrderID) > 0
ORDER BY SUM(soh.TotalDue) DESC;
"
top_customers <- dbGetQuery(con, customers_sql)

# Mostrar top 10 clientes
head(top_customers, 10)

# Scatter: orders vs avg order value (segmentación rápida)
p_customers <- ggplot(top_customers, aes(x = orders, y = total_spent)) +
  geom_point(aes(size = avg_order), alpha = 0.6) +
  scale_y_continuous(labels = scales::label_number(prefix = "S/ ", big.mark = ",")) +
  labs(title = "Clientes: número de órdenes vs gasto total", x = "Número de órdenes", y = "Gasto total (S/)", size = "Promedio por orden") +
  theme_minimal()
print(p_customers)
ggsave("clientes_scatter.png", p_customers, width = 9, height = 6)

# -------------------------
# 7) EFECTO DESCUENTO (por producto)
# -------------------------
discount_sql <- "
SELECT TOP 200 p.ProductID, p.Name AS ProductName,
       SUM(sod.OrderQty) AS qty,
       SUM(sod.LineTotal) AS sales,
       AVG(sod.UnitPriceDiscount) AS avg_discount
FROM Sales.SalesOrderDetail sod
JOIN Production.Product p ON sod.ProductID = p.ProductID
GROUP BY p.ProductID, p.Name
ORDER BY AVG(sod.UnitPriceDiscount) DESC;
"
disc_df <- dbGetQuery(con, discount_sql)

# Scatter: avg_discount vs sales
p_discount <- ggplot(disc_df, aes(x = avg_discount, y = sales, label = ProductName, size = qty)) +
  geom_point(alpha = 0.7) +
  geom_text_repel(data = disc_df %>% top_n(10, sales), size = 3) +
  scale_y_continuous(labels = scales::label_number(prefix = "S/ ", big.mark = ",")) +
  labs(title = "Efecto descuento: promedio descuento vs ventas por producto", x = "Descuento promedio (unit price discount)", y = "Ventas (S/)", size = "Cantidad vendida") +
  theme_minimal()
print(p_discount)
ggsave("descuento_vs_ventas.png", p_discount, width = 10, height = 6)

# -------------------------
# 8) ESTACIONALIDAD: PROMEDIO POR MES (across years)
# -------------------------
season_sql <- "
WITH monthly AS (
  SELECT YEAR(OrderDate) AS yr, MONTH(OrderDate) AS mth, SUM(TotalDue) AS monthly_sales
  FROM Sales.SalesOrderHeader
  GROUP BY YEAR(OrderDate), MONTH(OrderDate)
)
SELECT mth AS month, AVG(monthly_sales) AS avg_sales, SUM(monthly_sales) AS total_sales, COUNT(*) AS n_years
FROM monthly
GROUP BY mth
ORDER BY mth;
"
season_df <- dbGetQuery(con, season_sql) %>%
  mutate(month_name = month.abb[month])

p_season <- ggplot(season_df, aes(x = factor(month, levels = 1:12, labels = month.abb), y = avg_sales)) +
  geom_col() +
  scale_y_continuous(labels = scales::label_number(prefix = "S/ ", big.mark = ",")) +
  labs(title = "Promedio de ventas por mes (promedio entre años)", x = "Mes", y = "Ventas promedio (S/)") +
  theme_minimal()
print(p_season)
ggsave("estacionalidad_mensual.png", p_season, width = 10, height = 5)

# -------------------------
# 9) RETENCIÓN / REPETICIÓN (clientes con >1 orden)
# -------------------------
retention_sql <- "
WITH cust_orders AS (
  SELECT CustomerID, COUNT(SalesOrderID) AS cnt
  FROM Sales.SalesOrderHeader
  GROUP BY CustomerID
)
SELECT SUM(CASE WHEN cnt > 1 THEN 1 ELSE 0 END) AS repeat_customers,
       SUM(CASE WHEN cnt = 1 THEN 1 ELSE 0 END) AS one_time_customers,
       COUNT(*) AS customers_with_orders,
       CAST(SUM(CASE WHEN cnt > 1 THEN 1 ELSE 0 END) AS FLOAT) / COUNT(*) AS repeat_rate
FROM cust_orders;
"
ret <- dbGetQuery(con, retention_sql)
ret

# Pie chart rápido (one-time vs repeat)
ret_df <- data.frame(
  type = c("Repeat", "One-time"),
  count = c(ret$repeat_customers, ret$one_time_customers)
)
p_retention <- ggplot(ret_df, aes(x = "", y = count, fill = type)) +
  geom_col(width = 1) +
  coord_polar("y") +
  labs(title = "Clientes repetidores vs únicos") +
  theme_void()
print(p_retention)
ggsave("retencion_clientes.png", p_retention, width = 6, height = 6)


# CLEANUP: desconectar
dbDisconnect(con)
message("Script terminado. Gráficos guardados en el directorio de trabajo.")



