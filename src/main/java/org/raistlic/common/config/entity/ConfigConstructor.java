package org.raistlic.common.config.entity;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The annotation is to mark a static factory method or constructor that is preferred to be used
 * as the method to create a config entity instance.
 *
 * The annotated static factory method or constructor must have all its parameters marked with
 * {@link ConfigProperty}, unless no parameters needed.
 *
 * Although not restricted, it is preferred that only one of the static factory methods and constructors
 * being marked as {@link ConfigConstructor}, or otherwise which one to be used is undefined.
 *
 * @author lei.c (2015-12-22)
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.CONSTRUCTOR, ElementType.METHOD})
public @interface ConfigConstructor {

}
