-- Revisar primero si FrecuenciaDias ya existe en CRMNALCLOPT; no ejecutar en ese caso.
-- Compatible con el esquema MySQL/MariaDB usado por el CRM. No modifica filas históricas.
ALTER TABLE CRMNALCLOPT ADD COLUMN FrecuenciaDias DOUBLE NULL;
