package com.cosmetic.gg.controller;

import java.util.List;
import java.util.Objects;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import com.cosmetic.gg.common.constants.CommonConstant;
import com.cosmetic.gg.common.dto.Response;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.exception.GlobalException;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.entity.attribute.Attribute;
import com.cosmetic.gg.entity.attribute.ValueDetail;
import com.cosmetic.gg.entity.product.Brand;
import com.cosmetic.gg.model.attribute.ValueDetailModel;
import com.cosmetic.gg.service.attribute.AttributeService;
import com.cosmetic.gg.service.attribute.ValueDetailService;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(path = "/api/v1/value-detail")
@AllArgsConstructor
@CrossOrigin
public class ValueDetailController {

	private final ValueDetailService valueDetailService;
	
	@PostMapping(value = "")
	@ResponseStatus(value = HttpStatus.CREATED)
	public ResponseEntity<Response<?>> create(
			@RequestBody @Valid ValueDetailModel valueDetailModel,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			valueDetailModel.setId("");
			
			ValueDetail valueDetailEntity = valueDetailService.create(valueDetailModel);
			if (Objects.isNull(valueDetailEntity)) {
				ErrorCode errorCode = ErrorCode.FAILURE;
				return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(errorCode)),
						HttpStatus.BAD_REQUEST
				);
			}
			
			return new ResponseEntity<>(
				new Response<ValueDetail>().success(
						transId,
						valueDetailEntity
						),
				HttpStatus.CREATED
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PutMapping(value = "/{id}",
			produces = {MediaType.APPLICATION_JSON_VALUE},
			consumes = {MediaType.APPLICATION_JSON_VALUE})
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> update(
			@PathVariable(value = "id") String id,
			@RequestBody @Valid ValueDetailModel valueDetailModel,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			valueDetailModel.setId(id);
			
			List<Error> errors = valueDetailService.validator(valueDetailModel);
			if (!errors.isEmpty())
				return new ResponseEntity<>(
						new Response<Error[]>().error(
								transId,
								errors.toArray(new Error[0])
						),
						HttpStatus.BAD_REQUEST
				);
			
			ValueDetail valueDetailEntity = valueDetailService.update(valueDetailModel);
			if (Objects.isNull(valueDetailEntity)) {
				ErrorCode errorCode = ErrorCode.FAILURE;
				return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(errorCode)),
						HttpStatus.BAD_REQUEST
				);
			}
			
			return new ResponseEntity<>(
					new Response<ValueDetail>().success(
							transId,
							valueDetailEntity
							),
					HttpStatus.OK
					);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@GetMapping(value = "/{id}")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> getById(
		    @PathVariable(value = "id") String id,
		    HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			ValueDetailModel value;
			try {
				value = valueDetailService.detail(id);
			}catch(Exception ex) {
				value = null;
			}
			
			if (Objects.isNull(value))
		        return new ResponseEntity<>(
		          new Response<Error>().error(transId, new Error().builder(ErrorCode.NOT_FOUND)),
		          HttpStatus.NOT_FOUND
		        );
			
			return new ResponseEntity<>(
			        new Response<ValueDetailModel>().success(
			          transId,
			          value
			        ),
			        HttpStatus.OK
			      );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
}
