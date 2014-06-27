package com.insightfullogic.lambdabehave;

import com.insightfullogic.lambdabehave.testcases.fixtures.ShouldSetup;
import com.insightfullogic.lambdabehave.testcases.fixtures.ShouldSetupMulti;
import com.insightfullogic.lambdabehave.testcases.fixtures.ShouldSetupNone;
import org.junit.runner.RunWith;
import org.scalatest.WrapWith;
import org.mockito.InOrder;

import static com.insightfullogic.lambdabehave.BehaveRunner.runOnly;
import static com.insightfullogic.lambdabehave.Suite.describe;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.verifyZeroInteractions;

@RunWith(JunitSuiteRunner.class)
@WrapWith(ScalaTestWrapper.class)
public class SpecFixtureSpec {{

    describe("lambda behave spec fixtures", it -> {

        it.should("execute shouldSetup() before a spec1 and shouldTearDown() after a spec1", expect -> {
            runOnly(ShouldSetup.class);

            InOrder inOrder = inOrder(ShouldSetup.setup, ShouldSetup.spec, ShouldSetup.tearDown);
            inOrder.verify(ShouldSetup.setup).run();
            inOrder.verify(ShouldSetup.spec).accept(any());
            inOrder.verify(ShouldSetup.tearDown).run();
            inOrder.verifyNoMoreInteractions();
        });

        it.should("execute shouldSetup() before multiple specs and shouldTearDown() after multiple specs", expect -> {
            runOnly(ShouldSetupMulti.class);

            InOrder inOrder = inOrder(ShouldSetupMulti.setup, ShouldSetupMulti.spec1, ShouldSetupMulti.spec2, ShouldSetupMulti.tearDown);

            inOrder.verify(ShouldSetupMulti.setup).run();
            inOrder.verify(ShouldSetupMulti.spec1).accept(any());
            inOrder.verify(ShouldSetupMulti.tearDown).run();

            inOrder.verify(ShouldSetupMulti.setup).run();
            inOrder.verify(ShouldSetupMulti.spec2).accept(any());
            inOrder.verify(ShouldSetupMulti.tearDown).run();

            inOrder.verifyNoMoreInteractions();
        });

        it.should("not execute shouldSetup() or shouldTearDown() when there are no specs", expect -> {
            runOnly(ShouldSetupNone.class);

            verifyZeroInteractions(ShouldSetupNone.setup, ShouldSetupNone.tearDown);
        });

    });

}}
