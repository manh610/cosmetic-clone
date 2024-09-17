package com.cosmetic.gg.common.utils.datetime;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Calendar;
import java.util.Date;
import java.util.Objects;
import java.util.TimeZone;

import org.springframework.stereotype.Component;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

@Component
@Slf4j
public class DateUtils {

	public enum UNIT_TIME {
	    YEAR,
	    MONTH,
	    WEEK,
	    DAY,
	    HOUR,
	    MINUTE,
	    SECOND,
	    MILLISECOND
	  }

	  public static final String DDMMYYYY = "dd/MM/yyyy";
	  public static final String DDMMYYYY_HHMMSS = "dd/MM/yyyy HH:mm:ss";
	  public static final String YYYYMMDD_HHMMSS = "yyyy-MM-dd HH:mm:ss";
	  public static final String YYYYMMDD_T_HHMMSS = "yyyy-MM-dd'T'HH:mm:ss";
	  public static final String YYYYMMDD = "yyyy-MM-dd";
	  public static final String YYYYMMDD_HHMMSSZZ = "yyyy-MM-dd HH:mm:ssXXX";
	  public static final String FULL_DATE = "FULL_DATE";
	  public static final String YYYYMMDD_HHMM = "yyyyMMddHHmm";

	  @SneakyThrows
	  public static Date stringToDate(String dateString, String dateFormat) {
	    try {
	      return new SimpleDateFormat(dateFormat).parse(dateString);
	    } catch (ParseException e) {
	      String msg = String.format("%s with format \"%s\"", e.getMessage(), dateFormat);
	      log.error("stringToDate error: {}", msg);
	      throw new ParseException(msg, e.getErrorOffset());
	    }
	  }

	  public static String dateToString(Date date, String dateFormat) {
	    if (Objects.isNull(date)) {
	      return "";
	    }
	    return new SimpleDateFormat(dateFormat).format(date);
	  }

	  @SneakyThrows
	  public static LocalDateTime stringToLocalDate(String date, String pattern) {
	    try {
	      return LocalDateTime.parse(date, DateTimeFormatter.ofPattern(pattern));
	    } catch (Exception e) {
	      try {
	        LocalDate k = LocalDate.parse(date, DateTimeFormatter.ofPattern(YYYYMMDD));
	        return LocalDateTime.of(k, LocalTime.MIN);
	      } catch (Exception ef) {
	        log.error("stringToLocalDate error: {}, input: {}, pattern: {}", ef.getMessage(), date, pattern);
	        return null;
	      }
	    }
	  }

	  public static String localDateTimeToString(LocalDateTime date, String pattern) {
	    if (Objects.isNull(date)) {
	      return "";
	    }

	    DateTimeFormatter formatter = DateTimeFormatter.ofPattern(pattern);
	    return date.format(formatter);
	  }

	  public static LocalDateTime dateToLocalDateTime(Date dateToConvert) {
	    return dateToConvert.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();
	  }

	  public static LocalDateTime timeStampToLocalDateTime(long dateToConvert) {
	    return LocalDateTime.ofInstant(Instant.ofEpochMilli(dateToConvert), TimeZone.getDefault().toZoneId());
	  }

	  public static LocalDateTime calculatorLocalDateTime(String unitTime, long timeActive) {
	    switch (unitTime) {
	      case "y":
	        return calculatorLocalDateTime(UNIT_TIME.YEAR, timeActive);
	      case "mo":
	        return calculatorLocalDateTime(UNIT_TIME.MONTH, timeActive);
	      case "w":
	        return calculatorLocalDateTime(UNIT_TIME.WEEK, timeActive);
	      case "d":
	        return calculatorLocalDateTime(UNIT_TIME.DAY, timeActive);
	      case "h":
	        return calculatorLocalDateTime(UNIT_TIME.HOUR, timeActive);
	      case "s":
	        return calculatorLocalDateTime(UNIT_TIME.SECOND, timeActive);
	      case "ms":
	        return calculatorLocalDateTime(UNIT_TIME.MILLISECOND, timeActive);
	      case "m":
	      default:
	        return calculatorLocalDateTime(UNIT_TIME.MINUTE, timeActive);
	    }
	  }

	  public static LocalDateTime calculatorLocalDateTime(UNIT_TIME unitTime, long timeActive) {
	    switch (unitTime) {
	      case YEAR:
	        return LocalDateTime.now().plusYears(timeActive);
	      case MONTH:
	        return LocalDateTime.now().plusMonths(timeActive);
	      case WEEK:
	        return LocalDateTime.now().plusWeeks(timeActive);
	      case DAY:
	        return LocalDateTime.now().plusDays(timeActive);
	      case HOUR:
	        return LocalDateTime.now().plusHours(timeActive);
	      case SECOND:
	        return LocalDateTime.now().plusSeconds(timeActive);
	      case MILLISECOND:
	        return LocalDateTime.now().plusNanos(timeActive);
	      case MINUTE:
	      default:
	        return LocalDateTime.now().plusMinutes(timeActive);
	    }
	  }

	  public static Date calculatorDate(UNIT_TIME unitTime, int timeActive) {
	    try {
	      Calendar today = Calendar.getInstance();
	      long millisecond = 0;
	      switch (unitTime) {
	        case YEAR:
	          Calendar year = Calendar.getInstance();
	          year.add(Calendar.YEAR, timeActive);
	          millisecond = year.getTimeInMillis() - today.getTimeInMillis();
	          break;
	        case MONTH:
	          Calendar month = Calendar.getInstance();
	          month.add(Calendar.MONTH, timeActive);
	          millisecond = month.getTimeInMillis() - today.getTimeInMillis();
	          break;
	        case WEEK:
	          millisecond = timeActive * 1000L * 60 * 60 * 24 * 7;
	          break;
	        case DAY:
	          millisecond = timeActive * 1000L * 60 * 60 * 24;
	          break;
	        case HOUR:
	        default:
	          millisecond = timeActive * 1000L * 60 * 60;
	          break;
	        case MINUTE:
	          millisecond = timeActive * 1000L * 60;
	          break;
	        case SECOND:
	          millisecond = timeActive * 1000L;
	          break;
	        case MILLISECOND:
	          millisecond = timeActive;
	          break;
	      }

	      return new Date(System.currentTimeMillis() + millisecond);
	    } catch (Exception ex) {
	      log.error("Error calculator expired time", ex.getCause());
	      return new Date(System.currentTimeMillis());
	    }
	  }
}
