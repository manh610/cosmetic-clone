package com.cosmetic.gg.controller;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.springframework.http.HttpStatus;
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
import org.springframework.web.multipart.MultipartFile;

import com.cosmetic.gg.common.constants.CommonConstant;
import com.cosmetic.gg.common.dto.Response;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.exception.GlobalException;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.dto.response.product.ReviewResponse;
import com.cosmetic.gg.entity.product.Review;
import com.cosmetic.gg.service.product.ReviewService;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(path = "/api/v1/review")
@AllArgsConstructor
@CrossOrigin
public class ReviewController {

	private final ReviewService reviewService;
	
	@GetMapping(value = "/{id}")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> search(
		    @PathVariable(value = "id") String id,
		    @RequestParam(value = "star", required = false, defaultValue = "") Integer star,
		    HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			List<ReviewResponse> response = reviewService.search(id, star);
			return new ResponseEntity<>(
		        new Response<List<ReviewResponse>>().success(
		          transId,
		          response.size(),
		          response
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
			@RequestBody @Valid Review review,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			review.setId("");
			
			List<Error> errors = reviewService.validator(review);
			if(!errors.isEmpty())
				return new ResponseEntity<>(
					new Response<Error[]>().error(
							transId,
							errors.toArray(new Error[0])
					),
					HttpStatus.BAD_REQUEST
				);
			
			Review reviewEntity = reviewService.create(review);
			if (Objects.isNull(reviewEntity)) {
				ErrorCode errorCode = ErrorCode.FAILURE;
				return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(errorCode)),
						HttpStatus.BAD_REQUEST
				);
			}
			
			return new ResponseEntity<>(
				new Response<Review>().success(
						transId,
						reviewEntity
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
			Error error = reviewService.delete(id);
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
	
	@PutMapping(value = "/images/{id}")
	@ResponseStatus(value = HttpStatus.NO_CONTENT)
	public ResponseEntity<Response<?>> changImages(
			@PathVariable(value = "id") String id,
			@RequestParam("images") List<MultipartFile> images,
		    HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			ErrorCode errorCode = reviewService.changeImage(images, id);
			if (errorCode != ErrorCode.SUCCESS) {
		        return new ResponseEntity<>(
		          new Response<Error>().error(transId, new Error().builder(errorCode)),
		          HttpStatus.BAD_REQUEST
		        );
		      }
			return new ResponseEntity<>(
	            new Response<String>().success(
	              transId,
	              errorCode.getEn()
	            ),
	            HttpStatus.NO_CONTENT
	          );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
}
