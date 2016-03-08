package org.raistlic.common.config.entity;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The annotation is used to mark the members or parameters that are values to be fetched or
 * entities to be created from the configuration context.
 *
 * @author lei.c (2015-12-21)
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD, ElementType.PARAMETER, ElementType.METHOD})
public @interface ConfigProperty {

  /**
   * When {@code ""} , use the annotated field or method name, cannot be {@code ""} when annotated
   * on parameters.
   *
   * @return configuration property name or part.
   */
  String value() default "";
}
