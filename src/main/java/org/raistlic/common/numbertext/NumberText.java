package org.raistlic.common.numbertext;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * @author lei.c
 * @since 2014-11-24
 */
public interface NumberText {

  String toText(long number);

  String toText(BigInteger number) throws IllegalArgumentException;

  String toText(BigDecimal number) throws IllegalArgumentException;

  String toText(String number) throws IllegalArgumentException;
}
