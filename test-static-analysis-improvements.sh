#!/bin/bash

# Test script to validate the retry logic improvements in static analysis workflow
# This can be run locally to verify that the changes work as expected

echo "Testing Static Analysis Reliability Improvements"
echo "==============================================="

cd "$(dirname "$0")"

# Test 1: Format checking with timeout
echo "Test 1: Format checking with timeout..."
if timeout 300 ./check-format > /dev/null 2>&1; then
    echo "✓ Format check completed within timeout"
else
    echo "✗ Format check failed or timed out"
    exit 1
fi

# Test 2: PMD with --no-daemon flag
echo "Test 2: PMD analysis with CI configuration..."
cd libcobj
if ./gradlew pmdMain --no-daemon --console=plain > /dev/null 2>&1; then
    echo "✓ PMD completed successfully with --no-daemon"
else
    echo "✗ PMD failed"
    exit 1
fi

# Test 3: SpotBugs with --no-daemon flag
echo "Test 3: SpotBugs analysis with CI configuration..."
if ./gradlew spotbugsMain --no-daemon --console=plain > /dev/null 2>&1; then
    echo "✓ SpotBugs completed successfully with --no-daemon"
else
    echo "✗ SpotBugs failed"
    exit 1
fi

# Test 4: Verify gradle.properties configuration
echo "Test 4: Verify gradle.properties configuration..."
if grep -q "org.gradle.daemon=false" gradle.properties && grep -q "org.gradle.caching=true" gradle.properties; then
    echo "✓ Gradle configuration is properly set for CI"
else
    echo "✗ Gradle configuration is missing or incorrect"
    exit 1
fi

echo ""
echo "All tests passed! The static analysis improvements are working correctly."
echo "This should significantly reduce flakiness in CI environments."