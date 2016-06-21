import sys
import analyzer

if len(sys.argv) == 3:
    
    analyzer = analyzer.Analyzer()
    print analyzer.guess(sys.argv[1], sys.argv[2])
else:
    print "nothing"
