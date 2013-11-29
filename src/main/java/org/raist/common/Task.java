/*
 * Copyright 2013 Lei CHEN (raistlic@gmail.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.raist.common;

/**
 * A self executable task, that has a life cycle, and can be controlled by
 * calling its start, stop, resume and shutdown methods.
 *
 * @author Lei.C (13-11-9)
 */
public interface Task {

  public boolean start();

  public boolean stop();

  public boolean resume();

  public boolean shutdown();
}
