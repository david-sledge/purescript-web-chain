"use strict";

export const _setColSpecs = colSpecs => table => () => {table.colSpecs = colSpecs;};
export const _getColSpecs = table => () => table.colSpecs;

export const _setSortOrder = sortOrder => table => () => {table.sortOrder = sortOrder;};
export const _getSortOrder = table => () => table.sortOrder;

export const _setTableData = tableData => table => () => {table.tableData = tableData;};
export const _getTableData = table => () => table.tableData;

export const _setDataTableBody = tableDataBody => table => () => {table.tableDataBody = tableDataBody;};
export const _getDataTableBody = table => () => table.tableDataBody;

export const _setRowCountColumnClasses = rowCountColumnClasses => table => () => {table.rowCountColumnClasses = rowCountColumnClasses;};
export const _getRowCountColumnClasses = table => () => table.rowCountColumnClasses;
