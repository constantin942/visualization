package com.aiit.skyflying.common.utils;

import com.aiit.skyflying.common.constant.Const;
import org.codehaus.jackson.JsonParser;
import org.codehaus.jackson.JsonProcessingException;
import org.codehaus.jackson.map.DeserializationContext;
import org.codehaus.jackson.map.JsonDeserializer;
import org.springframework.util.StringUtils;

import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
/**
 * 解决@RequestBody接收json数据，Jackson 反序列化Date格式
 * @author scott
 *
 */
public class CustomJsonDateDeserializer extends JsonDeserializer<Date> {
	private SimpleDateFormat datetimeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
	private SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");

	@Override
	public Date deserialize(JsonParser jp, DeserializationContext ctxt) throws IOException, JsonProcessingException {
		String text = jp.getText();

		if (StringUtils.hasText(text)) {
			try {
				if (text.indexOf(Const.COLON_EN) == -1 && text.length() == Const.NUMBER_TEN) {
					return this.dateFormat.parse(text);
				} else if (text.indexOf(Const.COLON_EN) > 0 && text.length() == Const.NUMBER_NINETEEN) {
					return this.datetimeFormat.parse(text);
				} else {
					throw new IllegalArgumentException("Could not parse date, date format is error ");
				}
			} catch (ParseException ex) {
				IllegalArgumentException iae = new IllegalArgumentException("Could not parse date: " + ex.getMessage());
				iae.initCause(ex);
				throw iae;
			}
		} else {
			return null;
		}
	}

}
