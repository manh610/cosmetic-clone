package com.cosmetic.gg.common.model;

import com.cosmetic.gg.common.enums.ErrorCode;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class Error {

	private Object code;
	private Message message;
	
	public Error builder(ErrorCode errorCode) {
		return new Error(errorCode.getCode(), new Message(errorCode.getVn(), errorCode.getEn()));
	}
	
	public Error builder(ErrorCode errorCode, String detailVn, String detailEn) {
		return new Error(
				errorCode.getCode(),
				new Message(
						String.format("%s. Chi tiết: %s", errorCode.getVn(), detailVn),
						String.format("%s. Detail: %s", errorCode.getEn(), detailEn)));
	}
}
