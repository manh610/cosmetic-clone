package com.cosmetic.gg.common.validator;

import java.util.stream.Collectors;

import org.springframework.context.support.DefaultMessageSourceResolvable;
import org.springframework.stereotype.Component;
import org.springframework.validation.BindingResult;

import com.cosmetic.gg.common.constants.CommonConstant;
import com.cosmetic.gg.common.dto.Response;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.exception.GlobalException;
import com.cosmetic.gg.common.model.Message;
import com.cosmetic.gg.common.model.Error;

import lombok.extern.slf4j.Slf4j;

@Component
@Slf4j
public class ModelValidator {

	public Response<Error> validateData(BindingResult bindingResult, String transId){
		//check authentication
		ErrorCode errorCode = ErrorCode.INVALID_DATA;
		try {
			if (bindingResult.hasErrors()) {
				String errorDetail = bindingResult.getAllErrors()
						.stream()
						.map(DefaultMessageSourceResolvable::getDefaultMessage)
						.collect(Collectors.joining("; "));
				
				return new Response<Error>().error(
						transId,
						new Error(
								errorCode.getCode(),
								new Message(
										errorCode.getVn(),
										String.format("%s: %s", errorCode.getEn(), errorDetail))));
			}
			return null;
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(errorCode, transId);
		}
	}
}
