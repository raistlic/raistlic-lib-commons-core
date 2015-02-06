package org.raistlic.common.numbertext;

import org.raistlic.common.Factory;

/**
 * @author lei.c
 * @since 2014-11-24
 */
public interface NumberTextForEnglishBuilder extends Factory<NumberText> {

  public static enum UpperCaseMode {

    UPPER_CASE_ALL,
    UPPER_CASE_FIRST_LETTER,
    LOWER_CASE,
    KEEP_ORIGINAL,
    ;
  }

  NumberTextForEnglishBuilder withMinusText(String minusText);

  NumberTextForEnglishBuilder withPointText(String pointText);

  NumberTextForEnglishBuilder withUpperCaseMode(UpperCaseMode mode);
}
