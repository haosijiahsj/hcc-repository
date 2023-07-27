package com.hcc.repository.core.utils;

import com.hcc.repository.annotation.IConverter;
import com.hcc.repository.core.constants.DbType;
import com.hcc.repository.core.convert.LocalDateConverter;
import com.hcc.repository.core.convert.LocalDateTimeConverter;
import com.hcc.repository.core.convert.LocalTimeConverter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.NumberUtils;

import java.math.BigDecimal;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

/**
 * JdbcUtils
 *
 * @author hushengjun
 * @date 2023/6/7
 */
@Slf4j
public class JdbcUtils {

    private JdbcUtils() {}

    /**
     * 从元数据中获取列名
     * @param resultSetMetaData
     * @param columnIndex
     * @return
     * @throws SQLException
     */
    public static String lookupColumnName(ResultSetMetaData resultSetMetaData, int columnIndex) throws SQLException {
        String name = resultSetMetaData.getColumnLabel(columnIndex);
        if (name == null || name.length() < 1) {
            name = resultSetMetaData.getColumnName(columnIndex);
        }

        return name;
    }

    /**
     * 根据类型获取结果值，从org.springframework.jdbc.support.JdbcUtils copy过来
     * @param rs
     * @param index
     * @param requiredType
     * @return
     * @throws SQLException
     */
    public static Object getResultSetValue(ResultSet rs, int index, Class<?> requiredType) throws SQLException {
        if (requiredType == null) {
            return getResultSetValue(rs, index);
        }

        Object value;
        if (String.class == requiredType) {
            return rs.getString(index);
        }
        else if (boolean.class == requiredType || Boolean.class == requiredType) {
            value = rs.getBoolean(index);
        }
        else if (byte.class == requiredType || Byte.class == requiredType) {
            value = rs.getByte(index);
        }
        else if (short.class == requiredType || Short.class == requiredType) {
            value = rs.getShort(index);
        }
        else if (int.class == requiredType || Integer.class == requiredType) {
            value = rs.getInt(index);
        }
        else if (long.class == requiredType || Long.class == requiredType) {
            value = rs.getLong(index);
        }
        else if (float.class == requiredType || Float.class == requiredType) {
            value = rs.getFloat(index);
        }
        else if (double.class == requiredType || Double.class == requiredType || Number.class == requiredType) {
            value = rs.getDouble(index);
        }
        else if (BigDecimal.class == requiredType) {
            return rs.getBigDecimal(index);
        }
        else if (java.sql.Date.class == requiredType) {
            return rs.getDate(index);
        }
        else if (java.sql.Time.class == requiredType) {
            return rs.getTime(index);
        }
        else if (java.sql.Timestamp.class == requiredType || java.util.Date.class == requiredType) {
            return rs.getTimestamp(index);
        }
        else if (byte[].class == requiredType) {
            return rs.getBytes(index);
        }
        else if (Blob.class == requiredType) {
            return rs.getBlob(index);
        }
        else if (Clob.class == requiredType) {
            return rs.getClob(index);
        }
        else if (requiredType.isEnum()) {
            Object obj = rs.getObject(index);
            if (obj instanceof String) {
                return obj;
            }
            else if (obj instanceof Number) {
                return NumberUtils.convertNumberToTargetClass((Number) obj, Integer.class);
            }
            else {
                return rs.getString(index);
            }
        }
        else {
            try {
                return rs.getObject(index, requiredType);
            }
            catch (AbstractMethodError err) {
                log.debug("JDBC driver does not implement JDBC 4.1 'getObject(int, Class)' method", err);
            }
            catch (SQLFeatureNotSupportedException ex) {
                log.debug("JDBC driver does not support JDBC 4.1 'getObject(int, Class)' method", ex);
            }
            catch (SQLException ex) {
                log.debug("JDBC driver has limited support for JDBC 4.1 'getObject(int, Class)' method", ex);
            }

            Object val = rs.getObject(index);
            // 上述抛异常后，说明当前驱动不支持java8日期，使用自定义converter支持
            IConverter<?, Object> converter = null;
            if (LocalTime.class == requiredType) {
                converter = new LocalTimeConverter();
            } else if (LocalDate.class == requiredType) {
                converter = new LocalDateConverter();
            } else if (LocalDateTime.class == requiredType) {
                converter = new LocalDateTimeConverter();
            }
            if (converter != null) {
                return converter.convertToAttribute(val);
            }

            // Fall back to getObject without type specification, again
            // left up to the caller to convert the value if necessary.
            return getResultSetValue(rs, index);
        }

        // Perform was-null check if necessary (for results that the JDBC driver returns as primitives).
        return rs.wasNull() ? null : value;
    }

    /**
     * 获取结果值
     * @param rs
     * @param index
     * @return
     * @throws SQLException
     */
    public static Object getResultSetValue(ResultSet rs, int index) throws SQLException {
        Object obj = rs.getObject(index);
        String className = null;
        if (obj != null) {
            className = obj.getClass().getName();
        }
        if (obj instanceof Blob) {
            Blob blob = (Blob) obj;
            obj = blob.getBytes(1, (int) blob.length());
        }
        else if (obj instanceof Clob) {
            Clob clob = (Clob) obj;
            obj = clob.getSubString(1, (int) clob.length());
        }
        else if ("oracle.sql.TIMESTAMP".equals(className) || "oracle.sql.TIMESTAMPTZ".equals(className)) {
            obj = rs.getTimestamp(index);
        }
        else if (className != null && className.startsWith("oracle.sql.DATE")) {
            String metaDataClassName = rs.getMetaData().getColumnClassName(index);
            if ("java.sql.Timestamp".equals(metaDataClassName) || "oracle.sql.TIMESTAMP".equals(metaDataClassName)) {
                obj = rs.getTimestamp(index);
            }
            else {
                obj = rs.getDate(index);
            }
        }
        else if (obj instanceof java.sql.Date) {
            if ("java.sql.Timestamp".equals(rs.getMetaData().getColumnClassName(index))) {
                obj = rs.getTimestamp(index);
            }
        }
        return obj;
    }

    /**
     * 从url中获取数据库类型
     * @param jdbcUrl
     * @return
     */
    public static DbType getDbType(String jdbcUrl) {
        Assert.isFalse(StrUtils.isEmpty(jdbcUrl), "Error: The jdbcUrl is Null, Cannot read database type");
        String url = jdbcUrl.toLowerCase();
        if (url.contains(":mysql:") || url.contains(":cobar:")) {
            return DbType.MYSQL;
        } else if (url.contains(":postgresql:")) {
            return DbType.POSTGRE_SQL;
        } else if (url.contains(":oracle:")) {
            return DbType.ORACLE;
        } else if (url.contains(":sqlserver2012:")) {
            return DbType.SQL_SERVER;
        } else {
            log.warn("The jdbcUrl is " + jdbcUrl + ", Cannot Read Database type or The Database's Not Supported!");
            return DbType.UNKNOWN;
        }
    }

}
