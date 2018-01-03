
// Run a set of examples and print the results to the console.
//
// runExamples ::
//  Array
//    { moduleName :: String
//    , examples :: Array
//      { title :: String
//      , examples :: Array
//          { actual :: String
//          , expected :: String
//          }
//      }
//    } ->
//  Unit
//
function runExamples(moduleExamples) {
  moduleExamples.forEach((m) => {
    m.examples.forEach((eg) => eg.examples.forEach(runExample));
  });
  printResults(moduleExamples);
}

// Given an example (i.e. an object with "actual" and "expected" properties),
// adds a Boolean "result" property which says whether the test passed, as well
// as a String "message" property in the case where the test failed.
function runExample(eg) {
  eg.result = (eg.expected === eg.actual);
  eg.message = "Expected " + eg.expected + ", got " + eg.actual;
}

function printResults(moduleExamples) {
  var total = 0;
  var passed = 0;
  moduleExamples.forEach((m) => {
    console.log(m.moduleName);
    m.examples.forEach((eg) => {
      var counter = 0;
      console.log("  " + eg.title);
      eg.examples.forEach((e) => {
        counter += 1;
        console.log("    " + counter + ": " + exampleMessage(e));
        if (e.result) {
          passed += 1;
        }
        total += 1;
      });
    });
  });
  console.log(passed + "/" + total + " tests passed.");
}

function exampleMessage(e) {
  if (e.result) {
    return "PASS";
  } else {
    return "FAIL: " + e.message;
  }
}

function main() {
  console.log(process.argv[2]);
  runExamples(require(process.argv[2]).main);
}

if (require.main === module) {
  main();
}
