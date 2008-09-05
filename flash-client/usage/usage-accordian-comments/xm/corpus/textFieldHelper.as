//
// Copyright 2004 Elvis Mehmedovic.
// http://emehmedovic.com/
//

class xm.corpus.textFieldHelper {

	function textFieldHelper() {

		var c = TextField.prototype;

		c.getxRightScroll = function () {
			return this.hscroll + this._width;
		}

		c.getxhscroll = function () {
			return this.hscroll + 1;
		}

		c.setxhscroll = function (p_scroll) {
			var b = this.background;
			this.hscroll = p_scroll - 1;
			if (b != this.background) {
				this.background = b;
			}
		}

		c.getxmaxhscroll = function () {
			return this.maxhscroll + 1;
		}

		c.addProperty("xrightScroll", c.getxRightScroll, null);
		c.addProperty("xhscroll", c.getxhscroll, c.setxhscroll);
		c.addProperty("xmaxhscroll", c.getxmaxhscroll, null);

	}

}