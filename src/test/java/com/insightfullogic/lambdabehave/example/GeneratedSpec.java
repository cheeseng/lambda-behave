package com.insightfullogic.lambdabehave.example;

import com.insightfullogic.lambdabehave.JunitSuiteRunner;
import com.insightfullogic.lambdabehave.ScalaTestWrapper;
import org.junit.runner.RunWith;
import org.scalatest.WrapWith;

import static com.insightfullogic.lambdabehave.Suite.describe;
import static com.insightfullogic.lambdabehave.generators.Example.asciiStrings;

@RunWith(JunitSuiteRunner.class)
@WrapWith(ScalaTestWrapper.class)
public class GeneratedSpec {{

    describe("a String", it -> {

        it.requires(100)
          .example(asciiStrings())
          .toShow("reversing a String twice returns the original String", (expect, str) -> {
              String same = new StringBuilder(str).reverse().reverse().toString();
              expect.that(same).isEqualTo(str);
          });

    });

}}
