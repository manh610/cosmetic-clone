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
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import com.cosmetic.gg.common.constants.CommonConstant;
import com.cosmetic.gg.common.dto.Response;
import com.cosmetic.gg.common.enums.EDeliveryType;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.exception.GlobalException;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.entity.address.DeliveryUnit;
import com.cosmetic.gg.service.address.DeliveryUnitService;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(path = "/api/v1/delivery-unit")
@AllArgsConstructor
@CrossOrigin
public class DeliveryUnitController {

	private final DeliveryUnitService deliveryUnitService;
	
	@GetMapping(value = "")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> search(
			@RequestParam(value = "keyword", required = false, defaultValue = "") String keyword,
			@RequestParam(value = "deliveryType", required = false, defaultValue = "") EDeliveryType deliveryType) {
		String transId = UUID.randomUUID().toString();
		try {
			List<DeliveryUnit> deliveryUnit = deliveryUnitService.search(keyword, deliveryType);
			return new ResponseEntity<>(
		        new Response<List<DeliveryUnit>>().success(
		          transId,
		          deliveryUnit.size(),
		          deliveryUnit
		        ),
		        HttpStatus.OK
		      );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PostMapping(value = "")
	@ResponseStatus(value = HttpStatus.CREATED)
	public ResponseEntity<Response<?>> create(
			@RequestBody @Valid DeliveryUnit deliveryUnit,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			deliveryUnit.setId("");
			
			List<Error> errors = deliveryUnitService.validator(deliveryUnit);
			if(!errors.isEmpty())
				return new ResponseEntity<>(
					new Response<Error[]>().error(
							transId,
							errors.toArray(new Error[0])
					),
					HttpStatus.BAD_REQUEST
				);
			
			DeliveryUnit deliveryUnitEntity = deliveryUnitService.create(deliveryUnit);
			if (Objects.isNull(deliveryUnit)) {
				ErrorCode errorCode = ErrorCode.FAILURE;
				return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(errorCode)),
						HttpStatus.BAD_REQUEST
				);
			}
			
			return new ResponseEntity<>(
				new Response<DeliveryUnit>().success(
						transId,
						deliveryUnitEntity
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
			@RequestBody @Valid DeliveryUnit deliveryUnit,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			deliveryUnit.setId(id);
			
			List<Error> errors = deliveryUnitService.validator(deliveryUnit);
			if (!errors.isEmpty())
				return new ResponseEntity<>(
						new Response<Error[]>().error(
								transId,
								errors.toArray(new Error[0])
						),
						HttpStatus.BAD_REQUEST
				);
			
			DeliveryUnit deliveryUnitEntity = deliveryUnitService.update(deliveryUnit);
			if (Objects.isNull(deliveryUnit)) {
				ErrorCode errorCode = ErrorCode.FAILURE;
				return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(errorCode)),
						HttpStatus.BAD_REQUEST
				);
			}
			
			return new ResponseEntity<>(
					new Response<DeliveryUnit>().success(
							transId,
							deliveryUnitEntity
							),
					HttpStatus.OK
					);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@DeleteMapping(value = "/{id}")
	@ResponseStatus(value = HttpStatus.NO_CONTENT)
	public ResponseEntity<Response<?>> delete(
			@PathVariable(value = "id") String id,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			Error error = deliveryUnitService.delete(id);
			if(!error.getCode().toString().equals(ErrorCode.SUCCESS.getCode()))
				return new ResponseEntity<>(
						new Response<Error>().error(transId, error),
						HttpStatus.BAD_REQUEST
				);
			
			return new ResponseEntity<>(
					new Response<String>().success(
							transId,
							ErrorCode.SUCCESS.getEn()
					),
					HttpStatus.NO_CONTENT
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
}
