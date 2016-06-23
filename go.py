import sys
import analyzer
import parser
import settings

if len(sys.argv) == 3:

    settings = settings.Settings()
    
    parser = parser.Parser()
    parser.cache_from(settings.class_data)
    
    analyzer = analyzer.Analyzer(parser)
    print analyzer.guess(sys.argv[1], sys.argv[2])
else:
    print "nothing"
