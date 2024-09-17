package com.cosmetic.gg.common.exception;

import javax.validation.constraints.NotNull;

import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.model.Message;
import com.cosmetic.gg.common.utils.string.StringUtils;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class GlobalException extends RuntimeException{

	private Error error;
	private String transId;
	
	@NotNull
	public static GlobalException builder(@NotNull ErrorCode errorCode, String transId) {
		Error error = new Error(errorCode.getCode(), new Message(errorCode.getVn(), errorCode.getEn()));
		return new GlobalException(error, transId);
	}
	
	@NotNull
	public static GlobalException builder(@NotNull ErrorCode errorCode, String detailVn, String detailEn, String transId) {
		Error error = new Error(
				errorCode.getCode(),
				new Message(
						Boolean.TRUE.equals(StringUtils.isNullOrEmpty(detailVn)) ?
				          errorCode.getVn() : String.format("%s: %s", errorCode.getVn(), detailVn),
				        Boolean.TRUE.equals(StringUtils.isNullOrEmpty(detailEn)) ?
				          errorCode.getEn() : String.format("%s: %s", errorCode.getEn(), detailEn)));
		return new GlobalException(error, transId);
	}
}
