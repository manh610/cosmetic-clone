package com.cosmetic.gg.controller;

import java.util.List;
import java.util.Objects;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
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
import com.cosmetic.gg.entity.attribute.ProductItem;
import com.cosmetic.gg.service.attribute.ProductItemService;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(path = "/api/v1/product-item")
@AllArgsConstructor
@CrossOrigin
public class ProductItemController {

	private final ProductItemService productItemService;
	
	@PostMapping(value = "")
	@ResponseStatus(value = HttpStatus.CREATED)
	public ResponseEntity<Response<?>> create(
			@RequestBody @Valid ProductItem productItem,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			productItem.setId("");
			
			List<Error> errors = productItemService.validator(productItem);
			if (!errors.isEmpty())
				return new ResponseEntity<>(
						new Response<Error[]>().error(
								transId,
								errors.toArray(new Error[0])
						),
						HttpStatus.BAD_REQUEST
				);
			
			ProductItem productItemEntity = productItemService.create(productItem);
			if (Objects.isNull(productItemEntity)) {
				ErrorCode errorCode = ErrorCode.FAILURE;
				return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(errorCode)),
						HttpStatus.BAD_REQUEST
				);
			}
			
			return new ResponseEntity<>(
				new Response<ProductItem>().success(
						transId,
						productItemEntity
						),
				HttpStatus.CREATED
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
			Error error = productItemService.delete(id);
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
