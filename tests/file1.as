package    l1.l2.l3



{
	public class ClassName1 extends C1 implements I1,I2, I3{
		/**
		* This is v1 description
		* 
		* Our variable has a very nice and lond description
		* 
		* The type is String
		*/
		public var v1:String;

		new ControlG

		public function ClassName1(a:uint, b:String) {}

		/**
		* v2 documentation
		*/
		var v2:Number;

		/**
		* v3 documentation is here with extra / symbols
		*/
		var v3:Object;
		var v3Star:*;
		var v4:Vector.<String>;
		var v5:Array = [];

		/**
		* Getter descriotion / with extra symbols
		*/
		override public function get getterTest0():uint {
			return 10;
		}

		private function set setterTest0(value:String):void {}

		private function get getterTest22(a:String, b:Value):void {}


		public override function get getterTest11():String {}
		
		/**
		* Plain function description
		*/
		function test1(p1:String, p2:String):void {}

		var inTheMiddleMember:ClassSomething;
		
		private function test2():Number {}
		function test3():C2 {

			if (true) {
				trace("something")
				var inLocal:uint = 30;
			}
			
			var l1:Object;
			var l2:Vector.<Number>;

			var g:Function = function ():void {
				var superLocal:uint = 10;
			}
		}

		protected var v6:C4;
		private var v7:int = 19;
	}
}