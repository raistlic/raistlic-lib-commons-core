package org.raistlic.common.config.entity;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @author lei.c (2015-12-21)
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD, ElementType.PARAMETER, ElementType.METHOD})
public @interface ConfigProperty {

  /**
   * When {@code ""} , use the annotated field or method name.
   *
   * @return configuration property name, prefixed by {@link ConfigEntity#path()} {@code + "."} .
   */
  String value() default "";
}
