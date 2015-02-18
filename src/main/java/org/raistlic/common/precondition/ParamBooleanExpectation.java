//package org.raistlic.common.precondition;
//
///**
// * @author Lei.C (2015-02-17)
// */
//class ParamBooleanExpectation extends ParamExpectation implements BooleanExpectation {
//
//  private boolean evaluative;
//
//  private String name;
//
//  ParamBooleanExpectation(boolean evaluative, String name) {
//
//    this.evaluative = evaluative;
//    this.name = name;
//  }
//
//  @Override
//  public void isTrue() {
//
//    isTrue(null);
//  }
//
//  @Override
//  public void isTrue(String message) {
//
//    if (!evaluative) {
//      if (message == null) {
//        message = name == null ? "" : "'" + name + "' should be true, but was " + evaluative;
//      }
//      error(message);
//    }
//  }
//
//  @Override
//  public void isFalse() {
//
//    isFalse(null);
//  }
//
//  @Override
//  public void isFalse(String message) {
//
//    if (evaluative) {
//      if (message == null) {
//        message = name == null ? "" : "'" + name + "' should be false, but was " + evaluative;
//      }
//      error(message);
//    }
//  }
//}
