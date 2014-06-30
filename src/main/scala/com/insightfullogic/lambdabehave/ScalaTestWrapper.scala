/*
 * Copyright 2001-2014 Artima, Inc.
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
package com.insightfullogic.lambdabehave

import org.scalatest._
import org.scalatest.lambdabehave.ScalaTestHelper._
import com.insightfullogic.lambdabehave.impl.reports.Specifiers
import java.util.stream.Collectors.toList
import collection.JavaConverters._
import org.scalatest.events._
import com.insightfullogic.lambdabehave.impl.reports.Result
import org.scalatest.exceptions.PayloadField
import collection.mutable.ListBuffer

class ScalaTestWrapper(clazz: Class[_]) extends org.scalatest.Suite { thisSuite =>

  val specifier = BehaveRunner.declareOnly(clazz)
  val children = specifier.completeBehaviours.collect(toList()).asScala

  override def suiteName: String = specifier.getSuiteName

  override def suiteId: String = clazz.getName

  override def expectedTestCount(filter: Filter): Int = children.size

  override def testNames: Set[String] = children.map(_.getDescription).toSet

  override def tags: Map[String, Set[String]] = Map.empty

  override protected def runTests(testName: Option[String], args: Args): Status = {
    if (testName == null)
      throw new NullPointerException("testName was null")
    if (args == null)
      throw new NullPointerException("args was null")

    import args._

    val theTestNames = testNames
    if (theTestNames.size > 0)
      checkChosenStyles(configMap, styleName)

    val stopRequested = stopper

    // Wrap any non-DispatchReporter, non-CatchReporter in a CatchReporter,
    // so that exceptions are caught and transformed
    // into error messages on the standard error stream.
    val report = wrapReporterIfNecessary(thisSuite, reporter)
    val newArgs = args.copy(reporter = report)
    
    val statusBuffer = new ListBuffer[Status]()

    // If a testName is passed to run, just run that, else run the tests returned
    // by testNames.
    testName match {

      case Some(tn) =>
        val (filterTest, ignoreTest) = filter(tn, tags, suiteId)
        if (!filterTest) {
          if (ignoreTest)
            reportTestIgnored(thisSuite, report, tracker, tn, tn, getEscapedIndentedTextForTest(tn, 1, true), Some(LineInFile(88, "TODO: How to get the location??")))
          else
            statusBuffer += runTest(tn, newArgs)
        }

      case None =>
        for ((tn, ignoreTest) <- filter(theTestNames, tags, suiteId)) {
          if (!stopRequested()) {
            if (ignoreTest)
              reportTestIgnored(thisSuite, report, tracker, tn, tn, getEscapedIndentedTextForTest(tn, 1, true), Some(LineInFile(88, "TODO: How to get the location??")))
            else
              statusBuffer += runTest(tn, newArgs)
          }
      }
    }
    new CompositeStatus(Set.empty ++ statusBuffer)
  }

  override protected def runTest(testName: String, args: Args): Status = {

    if (testName == null)
      throw new NullPointerException("testName was null")
    if (args == null)
      throw new NullPointerException("args was null")

    import args._

    val testChildren = children.filter(_.getDescription == testName)

    if (testChildren.length == 0)
      throw new IllegalArgumentException("Test '" + testName + "' not found.")

    val statusList = 
      testChildren.map { child =>
        val testStartTime = System.currentTimeMillis
        reportTestStarting(this, reporter, tracker, testName, testName, rerunner, Some(LineInFile(88, "TODO: How to get the location??")))

        val formatter = getEscapedIndentedTextForTest(testName, 1, true)

        val messageRecorderForThisTest = createMessageRecorder(reporter)

        val informerForThisTest = createMessageRecordingInformer(thisSuite, reporter, tracker, testName, messageRecorderForThisTest)

        val documenterForThisTest = createMessageRecordingDocumenter(thisSuite, reporter, tracker, testName, messageRecorderForThisTest)

        val report = child.checkCompleteBehaviour()
        report.getResult match {
          case Result.SUCCESS =>
            val duration = System.currentTimeMillis - testStartTime
            reportTestSucceeded(thisSuite, reporter, tracker, testName, testName, messageRecorderForThisTest.recordedEvents(false, false), duration, formatter, rerunner, Some(LineInFile(88, "TODO: How to get the location??")))
            SucceededStatus
          case Result.FAILURE | Result.ERROR =>
            val duration = System.currentTimeMillis - testStartTime
            reporter(TestFailed(tracker.nextOrdinal(), report.getMessage, thisSuite.suiteName, thisSuite.suiteId, Some(thisSuite.getClass.getName), testName, testName, messageRecorderForThisTest.recordedEvents(false, false), None, Some(duration), Some(formatter), Some(LineInFile(88, "TODO: How to get the location??")), thisSuite.rerunner, None))
            FailedStatus
        }
      }

    new CompositeStatus(statusList.toSet)
  }

}
